/*
 * Copyright 2006 by VMware, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER(S) OR AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of the copyright holder(s)
 * and author(s) shall not be used in advertising or otherwise to promote
 * the sale, use or other dealings in this Software without prior written
 * authorization from the copyright holder(s) and author(s).
 */

/*
 * vmwarectrl.c --
 *
 *      The implementation of the VMWARE_CTRL protocol extension that
 *      allows X clients to communicate with the driver.
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "dixstruct.h"
#include "extnsionst.h"
#include "randrstr.h"
#include <X11/X.h>
#include <X11/extensions/panoramiXproto.h>

#include "vmware.h"
#include "vmwarectrlproto.h"

#ifndef HAVE_XORG_SERVER_1_5_0
#include <xf86_ansic.h>
#include <xf86_libc.h>
#endif

/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlQueryVersion --
 *
 *      Implementation of QueryVersion command handler. Initialises and
 *      sends a reply.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Writes reply to client
 *
 *----------------------------------------------------------------------------
 */

static int
VMwareCtrlQueryVersion(ClientPtr client)
{
   xVMwareCtrlQueryVersionReply rep = { 0, };

   REQUEST_SIZE_MATCH(xVMwareCtrlQueryVersionReq);

   rep.type = X_Reply;
   rep.length = 0;
   rep.sequenceNumber = client->sequence;
   rep.majorVersion = VMWARE_CTRL_MAJOR_VERSION;
   rep.minorVersion = VMWARE_CTRL_MINOR_VERSION;
   if (client->swapped) {
      swaps(&rep.sequenceNumber);
      swapl(&rep.length);
      swapl(&rep.majorVersion);
      swapl(&rep.minorVersion);
   }
   WriteToClient(client, sizeof(xVMwareCtrlQueryVersionReply), (char *)&rep);

   return client->noClientException;
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlDoSetRes --
 *
 *      Set the custom resolution into the mode list.
 *
 *      This is done by alternately updating one of two dynamic modes. It is
 *      done this way because the server gets upset if you try to switch
 *      to a new resolution that has the same index as the current one.
 *
 * Results:
 *      TRUE on success, FALSE otherwise.
 *
 * Side effects:
 *      One dynamic mode will be updated if successful.
 *
 *----------------------------------------------------------------------------
 */

static Bool
VMwareCtrlDoSetRes(ScrnInfoPtr pScrn,
                   CARD32 x,
                   CARD32 y,
                   Bool resetXinerama)
{
   int modeIndex;
   DisplayModePtr mode;
   VMWAREPtr pVMWARE = VMWAREPTR(pScrn);

   if (pScrn && pScrn->modes) {
      VmwareLog(("DoSetRes: %d %d\n", x, y));

      if (resetXinerama) {
         free(pVMWARE->xineramaNextState);
         pVMWARE->xineramaNextState = NULL;
         pVMWARE->xineramaNextNumOutputs = 0;
      }

      /*
       * Don't resize larger than possible but don't
       * return an X Error either.
       */
      if (x > pVMWARE->maxWidth ||
          y > pVMWARE->maxHeight) {
         return TRUE;
      }

      /*
       * Find an dynamic mode which isn't current, and replace it with
       * the requested mode. Normally this will cause us to alternate
       * between two dynamic mode slots, but there are some important
       * corner cases to consider. For example, adding the same mode
       * multiple times, adding a mode that we never switch to, or
       * adding a mode which is a duplicate of a built-in mode. The
       * best way to handle all of these cases is to directly test the
       * dynamic mode against the current mode pointer for this
       * screen.
       */

      for (modeIndex = 0; modeIndex < NUM_DYN_MODES; modeIndex++) {
         /*
          * Initialise the dynamic mode if it hasn't been used before.
          */
         if (!pVMWARE->dynModes[modeIndex]) {
            pVMWARE->dynModes[modeIndex] = VMWAREAddDisplayMode(pScrn, "DynMode", 1, 1);
         }

         mode = pVMWARE->dynModes[modeIndex];
         if (mode != pScrn->currentMode) {
            break;
         }
      }

      mode->HDisplay = x;
      mode->VDisplay = y;

      return TRUE;
   } else {
      return FALSE;
   }
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlSetRes --
 *
 *      Implementation of SetRes command handler. Initialises and sends a
 *      reply.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Writes reply to client
 *
 *----------------------------------------------------------------------------
 */

static int
VMwareCtrlSetRes(ClientPtr client)
{
   REQUEST(xVMwareCtrlSetResReq);
   xVMwareCtrlSetResReply rep = { 0, };
   ScrnInfoPtr pScrn;
   ExtensionEntry *ext;

   REQUEST_SIZE_MATCH(xVMwareCtrlSetResReq);

   if (!(ext = CheckExtension(VMWARE_CTRL_PROTOCOL_NAME))) {
      return BadMatch;
   }

   pScrn = ext->extPrivate;
   if (pScrn->scrnIndex != stuff->screen) {
      return BadMatch;
   }

   if (!VMwareCtrlDoSetRes(pScrn, stuff->x, stuff->y, TRUE)) {
      return BadValue;
   }

   rep.type = X_Reply;
   rep.length = (sizeof(xVMwareCtrlSetResReply) - sizeof(xGenericReply)) >> 2;
   rep.sequenceNumber = client->sequence;
   rep.screen = stuff->screen;
   rep.x = stuff->x;
   rep.y = stuff->y;
   if (client->swapped) {
      swaps(&rep.sequenceNumber);
      swapl(&rep.length);
      swapl(&rep.screen);
      swapl(&rep.x);
      swapl(&rep.y);
   }
   WriteToClient(client, sizeof(xVMwareCtrlSetResReply), (char *)&rep);

   return client->noClientException;
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlDoSetTopology --
 *
 *      Set the custom topology and set a dynamic mode to the bounding box
 *      of the passed topology. If a topology is already pending, then do
 *      nothing but do not return failure.
 *
 * Results:
 *      TRUE on success, FALSE otherwise.
 *
 * Side effects:
 *      One dynamic mode and the pending xinerama state will be updated if
 *      successful.
 *
 *----------------------------------------------------------------------------
 */

static Bool
VMwareCtrlDoSetTopology(ScrnInfoPtr pScrn,
                        xXineramaScreenInfo *extents,
                        unsigned long number)
{
   VMWAREPtr pVMWARE = VMWAREPTR(pScrn);

   if (pVMWARE && pVMWARE->xinerama) { 
      VMWAREXineramaPtr xineramaState;
      short maxX = 0;
      short maxY = 0;
      size_t i;

      if (pVMWARE->xineramaNextState) {
         VmwareLog(("DoSetTopology: Aborting due to existing pending state\n"));
         return TRUE;
      }

      for (i = 0; i < number; i++) {
         maxX = MAX(maxX, extents[i].x_org + extents[i].width);
         maxY = MAX(maxY, extents[i].y_org + extents[i].height);
      }

      VmwareLog(("DoSetTopology: %d %d\n", maxX, maxY));

      xineramaState = (VMWAREXineramaPtr)calloc(number, sizeof(VMWAREXineramaRec));
      if (xineramaState) {
         memcpy(xineramaState, extents, number * sizeof (VMWAREXineramaRec));

         /*
          * Make this the new pending Xinerama state. Normally we'll
          * wait until the next mode switch in order to synchronously
          * push this state out to X clients and the virtual hardware.
          *
          * However, if we're already in the right video mode, there
          * will be no mode change. In this case, push it out
          * immediately.
          */
         free(pVMWARE->xineramaNextState);
         pVMWARE->xineramaNextState = xineramaState;
         pVMWARE->xineramaNextNumOutputs = number;

         if (maxX == pVMWARE->ModeReg.svga_reg_width &&
             maxY == pVMWARE->ModeReg.svga_reg_height) {

	    /*
	     * The annoyance here is that when we reprogram the
	     * SVGA device's monitor topology registers, it may
	     * rearrange those monitors on the host's screen, but they
	     * will still have the old contents. This might be
	     * correct, but it isn't guaranteed to match what's on X's
	     * framebuffer at the moment. So we'll send a
	     * full-framebuffer update rect afterwards.
	     */

            vmwareNextXineramaState(pVMWARE);
#ifdef HAVE_XORG_SERVER_1_2_0
            RRSendConfigNotify(pScrn->pScreen);
#endif
            vmwareSendSVGACmdUpdateFullScreen(pVMWARE);

            return TRUE;
         } else {
            return VMwareCtrlDoSetRes(pScrn, maxX, maxY, FALSE);
         }

      } else {
         return FALSE;
      }
   } else {
      return FALSE;
   }
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlSetTopology --
 *
 *      Implementation of SetTopology command handler. Initialises and sends a
 *      reply.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Writes reply to client
 *
 *----------------------------------------------------------------------------
 */

static int
VMwareCtrlSetTopology(ClientPtr client)
{
   REQUEST(xVMwareCtrlSetTopologyReq);
   xVMwareCtrlSetTopologyReply rep = { 0, };
   ScrnInfoPtr pScrn;
   ExtensionEntry *ext;
   xXineramaScreenInfo *extents;

   REQUEST_AT_LEAST_SIZE(xVMwareCtrlSetTopologyReq);

   if (!(ext = CheckExtension(VMWARE_CTRL_PROTOCOL_NAME))) {
      return BadMatch;
   }

   pScrn = ext->extPrivate;
   if (pScrn->scrnIndex != stuff->screen) {
      return BadMatch;
   }

   extents = (xXineramaScreenInfo *)(stuff + 1);
   if (!VMwareCtrlDoSetTopology(pScrn, extents, stuff->number)) {
      return BadValue;
   }

   rep.type = X_Reply;
   rep.length = (sizeof(xVMwareCtrlSetTopologyReply) - sizeof(xGenericReply)) >> 2;
   rep.sequenceNumber = client->sequence;
   rep.screen = stuff->screen;
   if (client->swapped) {
      swaps(&rep.sequenceNumber);
      swapl(&rep.length);
      swapl(&rep.screen);
   }
   WriteToClient(client, sizeof(xVMwareCtrlSetTopologyReply), (char *)&rep);

   return client->noClientException;
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlDispatch --
 *
 *      Dispatcher for VMWARE_CTRL commands. Calls the correct handler for
 *      each command type.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Side effects of individual command handlers.
 *
 *----------------------------------------------------------------------------
 */

static int
VMwareCtrlDispatch(ClientPtr client)
{
   REQUEST(xReq);

   switch(stuff->data) {
   case X_VMwareCtrlQueryVersion:
      return VMwareCtrlQueryVersion(client);
   case X_VMwareCtrlSetRes:
      return VMwareCtrlSetRes(client);
   case X_VMwareCtrlSetTopology:
      return VMwareCtrlSetTopology(client);
   }
   return BadRequest;
}


/*
 *----------------------------------------------------------------------------
 *
 * SVMwareCtrlQueryVersion --
 *
 *      Wrapper for QueryVersion handler that handles input from other-endian
 *      clients.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Side effects of unswapped implementation.
 *
 *----------------------------------------------------------------------------
 */

static int
SVMwareCtrlQueryVersion(ClientPtr client)
{
   REQUEST(xVMwareCtrlQueryVersionReq);
   REQUEST_SIZE_MATCH(xVMwareCtrlQueryVersionReq);

   swaps(&stuff->length);

   return VMwareCtrlQueryVersion(client);
}


/*
 *----------------------------------------------------------------------------
 *
 * SVMwareCtrlSetRes --
 *
 *      Wrapper for SetRes handler that handles input from other-endian
 *      clients.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Side effects of unswapped implementation.
 *
 *----------------------------------------------------------------------------
 */

static int
SVMwareCtrlSetRes(ClientPtr client)
{
   REQUEST(xVMwareCtrlSetResReq);
   REQUEST_SIZE_MATCH(xVMwareCtrlSetResReq);

   swaps(&stuff->length);
   swapl(&stuff->screen);
   swapl(&stuff->x);
   swapl(&stuff->y);

   return VMwareCtrlSetRes(client);
}


/*
 *----------------------------------------------------------------------------
 *
 * SVMwareCtrlSetTopology --
 *
 *      Wrapper for SetTopology handler that handles input from other-endian
 *      clients.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Side effects of unswapped implementation.
 *
 *----------------------------------------------------------------------------
 */

static int
SVMwareCtrlSetTopology(ClientPtr client)
{
   REQUEST(xVMwareCtrlSetTopologyReq);
   REQUEST_SIZE_MATCH(xVMwareCtrlSetTopologyReq);

   swaps(&stuff->length);
   swapl(&stuff->screen);
   swapl(&stuff->number);
   /* Each extent is a struct of shorts. */
   SwapRestS(stuff);

   return VMwareCtrlSetTopology(client);
}


/*
 *----------------------------------------------------------------------------
 *
 * SVMwareCtrlDispatch --
 *
 *      Wrapper for dispatcher that handles input from other-endian clients.
 *
 * Results:
 *      Standard response codes.
 *
 * Side effects:
 *      Side effects of individual command handlers.
 *
 *----------------------------------------------------------------------------
 */

static int
SVMwareCtrlDispatch(ClientPtr client)
{
   REQUEST(xReq);

   switch(stuff->data) {
   case X_VMwareCtrlQueryVersion:
      return SVMwareCtrlQueryVersion(client);
   case X_VMwareCtrlSetRes:
      return SVMwareCtrlSetRes(client);
   case X_VMwareCtrlSetTopology:
      return SVMwareCtrlSetTopology(client);
   }
   return BadRequest;
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrlResetProc --
 *
 *      Cleanup handler called when the extension is removed.
 *
 * Results:
 *      None
 *
 * Side effects:
 *      None
 *
 *----------------------------------------------------------------------------
 */

static void
VMwareCtrlResetProc(ExtensionEntry* extEntry)
{
   /* Currently, no cleanup is necessary. */
}


/*
 *----------------------------------------------------------------------------
 *
 * VMwareCtrl_ExitInit --
 *
 *      Initialiser for the VMWARE_CTRL protocol extension.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Protocol extension will be registered if successful.
 *
 *----------------------------------------------------------------------------
 */

void
VMwareCtrl_ExtInit(ScrnInfoPtr pScrn)
{
   ExtensionEntry *myext;

   if (!(myext = CheckExtension(VMWARE_CTRL_PROTOCOL_NAME))) {
      if (!(myext = AddExtension(VMWARE_CTRL_PROTOCOL_NAME, 0, 0,
                                 VMwareCtrlDispatch,
                                 SVMwareCtrlDispatch,
                                 VMwareCtrlResetProc,
                                 StandardMinorOpcode))) {
         xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
                    "Failed to add VMWARE_CTRL extension\n");
	 return;
      }

      /*
       * For now, only support one screen as that's all the virtual
       * hardware supports.
       */
      myext->extPrivate = pScrn;

      xf86DrvMsg(pScrn->scrnIndex, X_INFO,
                 "Initialized VMWARE_CTRL extension version %d.%d\n",
                 VMWARE_CTRL_MAJOR_VERSION, VMWARE_CTRL_MINOR_VERSION);
   }
}
