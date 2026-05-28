#' Add Interactivity and Export Controls to a visNetwork Bayesian Network
#'
#' @description
#' Enhances a `visNetwork` Bayesian network visualization with advanced
#' interactivity features for exploration and export. Adds UI controls for
#' zooming, node dragging, multiple selection, node editing, and custom buttons
#' for font resizing and exporting the network as PNG or SVG.
#'
#' @param obj A `visNetwork` object produced by functions such as
#'   `bn_to_netviz_prep()` or `visNetwork::visNetwork()`.
#'
#' @details
#' This function wraps `visNetwork::visInteraction()`, `visNetwork::visOptions()`,
#' and `visNetwork::visEvents()` to add the following capabilities:
#'
#' * **Interactive navigation** – drag nodes, pan the canvas, and zoom.
#' * **Node selection and editing** – highlight nearest nodes and edit node
#'   labels/values directly in the viewer.
#' * **Font size control** – floating “Font Size” button with a live slider.
#' * **Export buttons** – quick-download buttons for **PNG** and **SVG** versions
#'   of the rendered network.
#'
#' The function injects JavaScript via `visEvents(afterDrawing = "…")` to create
#' these interactive UI elements dynamically once the graph is rendered.
#'
#' @return
#' A modified `visNetwork` object with interactive and export-ready controls.
#'
#' @examples
#' \dontrun{
#' library(visNetwork)
#' nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
#' edges <- data.frame(from = c(1, 2), to = c(2, 3))
#' vis <- visNetwork(nodes, edges)
#' bn_visNetwork_deliverable_interactivity(vis)
#' }
#'
#' @seealso [visNetwork::visInteraction()], [visNetwork::visOptions()],
#'   [visNetwork::visEvents()]
#'
#' @export
bn_visNetwork_deliverable_interactivity <- function(obj, physics = TRUE, type = "none", key_json = NULL, key_width = 0.1, panel_ns = NULL, download_prefix = "network"){

  obj %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,       # allow moving nodes
      dragView  = TRUE,       # allow moving the canvas
      zoomView  = TRUE,       # allow zoom
      multiselect = TRUE,      # allow multiple selection
      navigationButtons = FALSE
    ) %>%
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = list(
        enabled = TRUE,
        style = "margin: 10px; padding: 0 10px; border: 1px solid rgb(204, 204, 204); border-radius: 6px; font-size: 13px; background: transparent; cursor: pointer; width: 130px; height: 34px; box-sizing: border-box;"
      ),
      manipulation = FALSE
    ) %>%
    visNetwork::visEvents(
      afterDrawing = paste0("
    function() {
      if(document.getElementById('fontButton')) return;

      var network = this;
      var layoutType = '", type, "';
      var panelNs = ", ifelse(is.null(panel_ns), "null", paste0("'", panel_ns, "'")), ";
      var downloadPrefix = '", download_prefix, "';
      var physicsEnabled = ", tolower(physics), ";
      network.setOptions({ physics: { enabled: physicsEnabled } });"
      , "

      // Inline SVG icons (no CDN dependency)
      var icons = {
        pencil: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 512 512\" fill=\"currentColor\"><path d=\"M410.3 231l11.3-11.3-33.9-33.9-62.1-62.1L291.7 89.8l-11.3 11.3-22.6 22.6L58.6 322.9c-10.4 10.4-18 23.3-22.2 37.4L1 480.7c-2.5 8.4-.2 17.5 6.1 23.7s15.3 8.5 23.7 6.1l120.3-35.4c14.1-4.2 27-11.8 37.4-22.2L387.7 253.7 410.3 231zM160 399.4l-9.1 22.7c-4 3.1-8.5 5.4-13.3 6.9L59.4 452l23-78.1c1.4-4.9 3.8-9.4 6.9-13.3l22.7-9.1v32c0 8.8 7.2 16 16 16h32zM362.7 18.7L348.3 33.2 325.7 55.8 314.3 67.1l33.9 33.9 62.1 62.1 33.9 33.9 11.3-11.3 22.6-22.6 14.5-14.5c25-25 25-65.5 0-90.5L453.3 18.7c-25-25-65.5-25-90.5 0z\"/></svg>',
        atom: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 512 512\" fill=\"currentColor\"><path d=\"M256 398c-8.8 0-16 7.2-16 16v48c0 8.8 7.2 16 16 16s16-7.2 16-16v-48c0-8.8-7.2-16-16-16zm0-284c8.8 0 16-7.2 16-16V50c0-8.8-7.2-16-16-16s-16 7.2-16 16v48c0 8.8 7.2 16 16 16zm0 30c-61.9 0-112 50.1-112 112s50.1 112 112 112 112-50.1 112-112-50.1-112-112-112zm0 176c-35.3 0-64-28.7-64-64s28.7-64 64-64 64 28.7 64-64-28.7 64-64 64zM398 256c0-8.8 7.2-16 16-16h48c8.8 0 16 7.2 16 16s-7.2 16-16 16h-48c-8.8 0-16-7.2-16-16zM114 256c0 8.8-7.2 16-16 16H50c-8.8 0-16-7.2-16-16s7.2-16 16-16h48c8.8 0 16 7.2 16 16z\"/></svg>',
        fileCode: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 384 512\" fill=\"currentColor\"><path d=\"M64 0C28.7 0 0 28.7 0 64v384c0 35.3 28.7 64 64 64h256c35.3 0 64-28.7 64-64V160H256c-17.7 0-32-14.3-32-32V0H64zm192 0v128h128L256 0zM153 289l-31 31 31 31c6.2 6.2 6.2 16.4 0 22.6s-16.4 6.2-22.6 0l-42-42c-6.2-6.2-6.2-16.4 0-22.6l42-42c6.2-6.2 16.4-6.2 22.6 0s6.2 16.4 0 22.6zm76-45l42 42c6.2 6.2 6.2 16.4 0 22.6l-42 42c-6.2 6.2-16.4 6.2-22.6 0s-6.2-16.4 0-22.6l31-31-31-31c-6.2-6.2-6.2-16.4 0-22.6s16.4-6.2 22.6 0z\"/></svg>',
        image: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 512 512\" fill=\"currentColor\"><path d=\"M0 96c0-35.3 28.7-64 64-64h384c35.3 0 64 28.7 64 64v320c0-35.3-28.7 64-64 64H64c-35.3 0-64-28.7-64-64V96zM323.8 202.5c-4.5-6.6-11.9-10.5-19.8-10.5s-15.4 3.9-19.8 10.5l-87 127.6L170.7 297c-4.6-5.7-11.5-9-18.7-9s-14.2 3.3-18.7 9l-64 80c-5.8 7.2-6.9 17.1-2.9 25.4s12.4 13.6 21.6 13.6h96 32H424c8.9 0 17.1-4.9 21.2-12.8s3.6-17.4-1.4-24.7l-120-176zM112 192a48 48 0 1 0 0-96 48 48 0 1 0 0 96z\"/></svg>',
        save: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 448 512\" fill=\"currentColor\"><path d=\"M64 32C28.7 32 0 60.7 0 96v320c0 35.3 28.7 64 64 64h320c35.3 0 64-28.7 64-64V173.3c0-17-6.7-33.3-18.7-45.3L352 50.7C340 38.7 323.7 32 306.7 32H64zm0 96c0-17.7 14.3-32 32-32h192c17.7 0 32 14.3 32 32v64c0 17.7-14.3 32-32 32H96c-17.7 0-32-14.3-32-32v-64zm128 256a64 64 0 1 0 0-128 64 64 0 1 0 0 128z\"/></svg>',
        folderOpen: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 576 512\" fill=\"currentColor\"><path d=\"M88.7 223.8L0 375.8V96c0-35.3 28.7-64 64-64h117.5c16.2 0 31.8 6.5 43.3 17.9l7.4 7.4C241.3 66.4 253.4 72 266 72H400c35.3 0 64 28.7 64 64v32H128c-16 0-30.7 9.2-37.3 23.8zM512 196.4L476.4 420.3c-4 22.4-23.3 38.6-46 38.6H88.9c-22.7 0-42-16.2-46-38.6L7 196.4C5.6 188.4 11.5 181 19.7 181H492.3c8.2 0 14.1 7.4 12.7 15.4z\"/></svg>',
        chevronUp: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 448 512\" fill=\"currentColor\"><path d=\"M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z\"/></svg>',
        chevronDown: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 448 512\" fill=\"currentColor\"><path d=\"M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z\"/></svg>',
        chevronLeft: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 320 512\" fill=\"currentColor\"><path d=\"M9.4 233.4c-12.5 12.5-12.5 32.8 0 45.3l160 160c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3L77.3 256 214.6 118.6c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0l-160 160z\"/></svg>',
        chevronRight: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 320 512\" fill=\"currentColor\"><path d=\"M310.6 233.4c12.5 12.5 12.5 32.8 0 45.3l-160 160c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3L242.7 256 105.4 118.6c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0l160 160z\"/></svg>',
        minus: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 448 512\" fill=\"currentColor\"><path d=\"M432 256c0 17.7-14.3 32-32 32H48c-17.7 0-32-14.3-32-32s14.3-32 32-32h352c17.7 0 32 14.3 32 32z\"/></svg>',
        plus: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 448 512\" fill=\"currentColor\"><path d=\"M256 80c0-17.7-14.3-32-32-32s-32 14.3-32 32v144H48c-17.7 0-32 14.3-32 32s14.3 32 32 32h144v144c0 17.7 14.3 32 32 32s32-14.3 32-32V288h144c17.7 0 32-14.3 32-32s-14.3-32-32-32H256V80z\"/></svg>',
        expand: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 448 512\" fill=\"currentColor\"><path d=\"M32 32C14.3 32 0 46.3 0 64v96c0 17.7 14.3 32 32 32s32-14.3 32-32V96h64c17.7 0 32-14.3 32-32s-14.3-32-32-32H32zM64 416H32V352c0-17.7-14.3-32-32-32s-32 14.3-32 32v96c0 17.7 14.3 32 32 32h96c17.7 0 32-14.3 32-32s-14.3-32-32-32zm320-320h-64c-17.7 0-32 14.3-32 32s14.3 32 32 32h64v64c0 17.7 14.3 32 32 32s32-14.3 32-32V64c0-17.7-14.3-32-32-32zm32 320V352c0-17.7-14.3-32-32-32s-32 14.3-32 32v64h-64c-17.7 0-32 14.3-32 32s14.3 32 32 32h96c17.7 0 32-14.3 32-32z\"/></svg>',
        fullscreen: '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 512 512\" fill=\"currentColor\"><path d=\"M344 0H488c13.3 0 24 10.7 24 24V168c0 9.7-5.8 18.5-14.8 22.2s-19.3 1.7-26.2-5.2l-39-39-87 87c-9.4 9.4-24.6 9.4-33.9 0l-32-32c-9.4-9.4-9.4-24.6 0-33.9l87-87L327 41c-6.9-6.9-8.9-17.2-5.2-26.2S334.3 0 344 0zM168 512H24c-13.3 0-24-10.7-24-24V344c0-9.7 5.8-18.5 14.8-22.2s19.3-1.7 26.2 5.2l39 39 87-87c9.4-9.4 24.6-9.4 33.9 0l32 32c9.4 9.4 9.4 24.6 0 33.9l-87 87 39 39c6.9 6.9 8.9 17.2 5.2 26.2s-12.5 14.8-22.2 14.8z\"/></svg>'
      };

      // shared button dimensions
      var btnW = 130;
      var btnH = 30;
      var btnGap = 6;
      var btnStyle = 'width:' + btnW + 'px;height:' + btnH + 'px;padding:0 10px;background:transparent;color:var(--ndr-text);border:1px solid var(--ndr-border);border-radius:6px;cursor:pointer;font-size:13px;box-sizing:border-box;display:flex;align-items:center;justify-content:center;gap:4px;flex-shrink:0;white-space:nowrap;';

      // right-side button container — flex COLUMN at the top-right
      // corner so Font Size / Physics / Download SVG / Download PNG
      // stack vertically. `align-items: flex-end` keeps them right-
      // edge aligned (matters if button widths ever differ).
      // Dropped the `left: 220px` constraint that was needed when the
      // buttons were horizontal (so they didn\'t overlap left-side
      // controls) — irrelevant for a vertical stack pinned to the right.
      var rightBar = document.createElement('div');
      rightBar.id = 'rightButtonBar';
      rightBar.style.cssText = 'position:fixed;top:10px;right:10px;z-index:9999;display:flex;flex-direction:column;align-items:flex-end;gap:' + btnGap + 'px;pointer-events:none;';
      document.body.appendChild(rightBar);

      // style the nodesIdSelection select to match button dimensions
      var selStyle = document.createElement('style');
      selStyle.textContent = '.vis-configuration-wrapper select, div[style*=\"nodesIdSelection\"] select, .vis-network select { width:' + btnW + 'px !important; height:' + btnH + 'px !important; padding:0 10px !important; font-size:13px !important; color:var(--ndr-text) !important; background:var(--ndr-card-bg) !important; border:1px solid var(--ndr-border) !important; border-radius:6px !important; box-sizing:border-box !important; max-height:300px !important; } .vis-configuration-wrapper select:focus, div[style*=\"nodesIdSelection\"] select:focus, .vis-network select:focus { border-color:var(--ndr-accent) !important; box-shadow:0 0 0 0.2rem var(--ndr-focus) !important; outline:0 !important; } #rightButtonBar > button { pointer-events:auto; } .nav-ctrl:hover{background:var(--ndr-secondary-bg)!important;border-color:var(--ndr-border)!important;} .nav-ctrl:active{background:color-mix(in srgb, var(--ndr-secondary-bg) 80%, black)!important;}';
      document.head.appendChild(selStyle);

      // -------------------- Custom Navigation Controls --------------------
      var navSize = 30;
      var navGap = 4;
      var navBtnStyle = 'width:' + navSize + 'px;height:' + navSize + 'px;border-radius:50%;border:1px solid var(--ndr-border);background:var(--ndr-card-bg);color:var(--ndr-text);cursor:pointer;display:flex;align-items:center;justify-content:center;font-size:12px;padding:0;';
      function makeNavBtn(svgIcon) {
        var b = document.createElement('button');
        b.className = 'nav-ctrl';
        b.innerHTML = svgIcon;
        b.style.cssText = navBtnStyle;
        return b;
      }
      function panView(dx, dy) {
        var pos = network.getViewPosition();
        var s = network.getScale();
        network.moveTo({ position: { x: pos.x + dx/s, y: pos.y + dy/s } });
      }

      // directional pad (bottom-left)
      var navPad = document.createElement('div');
      navPad.style.cssText = 'position:fixed;bottom:10px;left:10px;z-index:9999;';
      var navRow1 = document.createElement('div');
      navRow1.style.cssText = 'display:flex;justify-content:center;margin-bottom:' + navGap + 'px;';
      var navRow2 = document.createElement('div');
      navRow2.style.cssText = 'display:flex;gap:' + navGap + 'px;';

      var upBtn = makeNavBtn(icons.chevronUp);
      upBtn.addEventListener('click', function() { panView(0, -120); });
      navRow1.appendChild(upBtn);
      var leftBtn = makeNavBtn(icons.chevronLeft);
      leftBtn.addEventListener('click', function() { panView(-120, 0); });
      navRow2.appendChild(leftBtn);
      var downBtn = makeNavBtn(icons.chevronDown);
      downBtn.addEventListener('click', function() { panView(0, 120); });
      navRow2.appendChild(downBtn);
      var rightBtn = makeNavBtn(icons.chevronRight);
      rightBtn.addEventListener('click', function() { panView(120, 0); });
      navRow2.appendChild(rightBtn);

      navPad.appendChild(navRow1);
      navPad.appendChild(navRow2);
      document.body.appendChild(navPad);

      // zoom controls (bottom-right)
      var zoomPad = document.createElement('div');
      zoomPad.style.cssText = 'position:fixed;bottom:10px;right:10px;z-index:9999;display:flex;gap:' + navGap + 'px;';
      var zoomOutBtn = makeNavBtn(icons.minus);
      zoomOutBtn.addEventListener('click', function() {
        network.moveTo({ scale: network.getScale() * 0.7, animation: { duration: 200 } });
      });
      zoomPad.appendChild(zoomOutBtn);
      var zoomInBtn = makeNavBtn(icons.plus);
      zoomInBtn.addEventListener('click', function() {
        network.moveTo({ scale: network.getScale() * 1.4, animation: { duration: 200 } });
      });
      zoomPad.appendChild(zoomInBtn);
      var fitBtn = makeNavBtn(icons.expand);
      fitBtn.title = 'Fit to View';
      fitBtn.addEventListener('click', function() { network.fit({ animation: { duration: 300 } }); });
      zoomPad.appendChild(fitBtn);

      // Fullscreen toggle — rightmost button in the bottom-right zoomPad.
      // Iframe must be marked allowfullscreen by the parent for
      // requestFullscreen to succeed in a sandboxed context. Camera state
      // (scale / position) is left for vis.js to handle natively on
      // resize — interfering with moveTo here caused flicker on entry
      // and a wrong-scale snap on exit.
      // Remember the pre-fullscreen camera RIGHT NOW (before entering). By
      // the time fullscreenchange fires, vis has already re-fit for the big
      // viewport, so capturing then saves the wrong (fullscreen) camera.
      var savedView = null;
      var fsBtn = makeNavBtn(icons.fullscreen);
      fsBtn.title = 'Toggle Fullscreen';
      fsBtn.addEventListener('click', function() {
        if (document.fullscreenElement) {
          var exit = document.exitFullscreen ||
                     document.webkitExitFullscreen ||
                     document.mozCancelFullScreen ||
                     document.msExitFullscreen;
          if (exit) { try { exit.call(document); } catch(e) {} }
        } else {
          try { savedView = { scale: network.getScale(), position: network.getViewPosition() }; }
          catch (e) { savedView = null; }
          var el = document.documentElement;
          var req = el.requestFullscreen || el.webkitRequestFullscreen ||
                    el.mozRequestFullScreen || el.msRequestFullscreen;
          if (req) { try { req.call(el); } catch(e) {} }
        }
      });
      zoomPad.appendChild(fsBtn);

      // On exit, HOLD the saved pre-fullscreen camera through the shrink.
      // The trace showed vis's autoResize rescales proportionally on each
      // intermediate resize and DRIFTS (ended s=0.37 pos x=1527 instead of
      // s=0.54 x=7) — that drift is the off-center beat that no single fit/
      // restore could hide. Re-applying the FIXED saved camera on every
      // resize during exit keeps the framing rock-steady (mine wins each
      // frame; fixed target = no jump), so the graph holds its normal view
      // while the window shrinks around it. Detach after the transition so
      // ordinary window resizes are untouched. fit() fallback if no save.
      var onFsChange = function() {
        if (document.fullscreenElement || document.webkitFullscreenElement) return;
        var hold = savedView
          ? function() { try { network.moveTo({ scale: savedView.scale, position: savedView.position }); } catch (e) {} }
          : function() { try { network.fit(); } catch (e) {} };
        hold();
        window.addEventListener('resize', hold);
        setTimeout(function() { window.removeEventListener('resize', hold); hold(); }, 600);
      };
      document.addEventListener('fullscreenchange', onFsChange);
      document.addEventListener('webkitfullscreenchange', onFsChange);

      document.body.appendChild(zoomPad);

      // -------------------- Custom Select Dropdown --------------------
      // the native select is a sibling of .vis-network, not inside it
      var idSel = document.querySelector('select');
      if (idSel) {
        // hide the native select (and its label wrapper only if it does not contain the network)
        idSel.style.display = 'none';
        var selParent = idSel.parentNode;
        if (selParent && selParent !== document.body && !selParent.querySelector('.vis-network')) {
          selParent.style.display = 'none';
        }

        // build node list from the network data directly
        var nodeList = network.body.data.nodes.get().sort(function(a,b) {
          return (a.label || a.id.toString()).localeCompare(b.label || b.id.toString());
        });

        var ddWrap = document.createElement('div');
        ddWrap.style.cssText = 'position:fixed;top:10px;left:10px;z-index:100000;font-family:-apple-system,BlinkMacSystemFont,sans-serif;';
        // Side-control width: matches the customLegend\'s max-width
        // (200px) so the Select-by-ID button and the legend below it
        // read as the same column of controls.
        var sideW = 200;
        var ddBtn = document.createElement('button');
        ddBtn.id = 'idSelectBtn';
        ddBtn.style.cssText = 'width:' + sideW + 'px;height:' + btnH + 'px;padding:0 10px;background:transparent;color:var(--ndr-text);border:1px solid var(--ndr-border);border-radius:6px;cursor:pointer;font-size:13px;box-sizing:border-box;text-align:left;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;';
        ddBtn.textContent = 'Select by ID';
        ddWrap.appendChild(ddBtn);

        var ddPanel = document.createElement('div');
        // Dropdown panel matches the button width so it doesn\'t look
        // narrower than its trigger.
        ddPanel.style.cssText = 'display:none;position:absolute;top:' + (btnH + 4) + 'px;left:0;min-width:' + sideW + 'px;max-height:300px;overflow-y:auto;background:white;border:1px solid #ccc;border-radius:6px;box-shadow:0 2px 8px rgba(0,0,0,0.15);padding:4px 0;';

        nodeList.forEach(function(nd) {
          var item = document.createElement('div');
          item.textContent = nd.label || nd.id;
          item.style.cssText = 'padding:6px 10px;cursor:pointer;font-size:13px;white-space:nowrap;';
          item.addEventListener('mouseenter', function() { item.style.background = 'var(--ndr-secondary-bg)'; });
          item.addEventListener('mouseleave', function() { item.style.background = 'transparent'; });
          item.addEventListener('click', function() {
            // drive the hidden native select to trigger highlightNearest
            idSel.value = nd.id;
            if (typeof idSel.onchange === 'function') {
              idSel.onchange();
            } else {
              idSel.dispatchEvent(new Event('change', {bubbles:true}));
            }
            network.focus(nd.id, { scale: 1, animation: { duration: 300 } });
            ddBtn.textContent = item.textContent;
            ddPanel.style.display = 'none';
          });
          ddPanel.appendChild(item);
        });

        ddWrap.appendChild(ddPanel);
        document.body.appendChild(ddWrap);

        ddBtn.addEventListener('click', function(e) {
          ddPanel.style.display = ddPanel.style.display === 'none' ? 'block' : 'none';
          e.stopPropagation();
        });
        document.addEventListener('click', function(e) {
          if (!ddWrap.contains(e.target)) ddPanel.style.display = 'none';
        });

        // sync button text when nodes are selected/deselected via the graph
        network.on('selectNode', function(params) {
          if (params.nodes.length === 1) {
            var nd = network.body.data.nodes.get(params.nodes[0]);
            ddBtn.textContent = nd.label || nd.id;
          }
        });
        network.on('deselectNode', function() {
          ddBtn.textContent = 'Select by ID';
        });
      }

      // -------------------- Font Size Button --------------------
      var btn = document.createElement('button');
      btn.id = 'fontButton';
      btn.className = 'vis-button';
      btn.innerHTML = '' + icons.pencil + ' Font Size';
      btn.style.cssText = btnStyle;
      rightBar.appendChild(btn);

      // Slider container (hidden initially)
      var sliderContainer = document.createElement('div');
      sliderContainer.style.position = 'fixed';
      sliderContainer.style.top = '50px';
      sliderContainer.style.zIndex = 9999;
      sliderContainer.style.padding = '6px 10px';
      sliderContainer.style.background = 'rgba(255,255,255,0.95)';
      sliderContainer.style.border = '1px solid #ccc';
      sliderContainer.style.borderRadius = '6px';
      sliderContainer.style.display = 'none';
      document.body.appendChild(sliderContainer);

      // Slider label
      var label = document.createElement('label');
      label.innerHTML = 'Font size: ';
      sliderContainer.appendChild(label);

      // Slider
      var slider = document.createElement('input');
      slider.type = 'range';
      slider.min = 10;
      slider.max = 60;
      var defaultFontSize = (layoutType === 'gravity') ? 30 : 20;
      slider.value = defaultFontSize;
      slider.style.marginRight = '6px';
      sliderContainer.appendChild(slider);

      // Current value display
      var span = document.createElement('span');
      span.innerHTML = slider.value;
      sliderContainer.appendChild(span);

      // Toggle slider visibility below the button
      btn.addEventListener('click', function(e){
        if(sliderContainer.style.display === 'none') {
          var rect = btn.getBoundingClientRect();
          sliderContainer.style.top = (rect.bottom + 6) + 'px';
          sliderContainer.style.left = rect.left + 'px';
          sliderContainer.style.display = 'block';
          span.style.display = 'inline';
        } else {
          sliderContainer.style.display = 'none';
          span.style.display = 'none';
        }
        e.stopPropagation();
      });

      // Hide slider when clicking outside
      document.addEventListener('click', function(e){
        if(!sliderContainer.contains(e.target) && e.target !== btn){
          sliderContainer.style.display = 'none';
          span.style.display = 'none';
        }
      });

      sliderContainer.addEventListener('click', function(e){
        e.stopPropagation();
      });

      // Initialize node fonts
      var nodesData = network.body.data.nodes.get();
      nodesData.forEach(function(n){ if(!n.font) n.font={size:defaultFontSize}; });
      network.body.data.nodes.update(nodesData);

      // Slider event: update all nodes
      slider.addEventListener('input', function(){
        var size = parseInt(this.value);
        span.innerHTML = size;
        var nodesData = network.body.data.nodes.get();
        nodesData.forEach(function(n){ n.font.size = size; });
        network.body.data.nodes.update(nodesData);
      });

      // -------------------- Physics Toggle Button --------------------
      if (layoutType !== 'charge') {
        var physBtn = document.createElement('button');
        physBtn.id = 'physicsButton';
        physBtn.className = 'vis-button';
        physBtn.innerHTML = physicsEnabled
          ? '' + icons.atom + ' Physics: On'
          : '' + icons.atom + ' Physics: Off';
        physBtn.style.cssText = btnStyle + (physicsEnabled ? 'background:color-mix(in srgb, var(--ndr-accent) 16%, transparent);' : '');
        rightBar.appendChild(physBtn);

        physBtn.addEventListener('click', function() {
          physicsEnabled = !physicsEnabled;
          network.setOptions({ physics: { enabled: physicsEnabled } });
          if (physicsEnabled) {
            physBtn.innerHTML = '' + icons.atom + ' Physics: On';
            physBtn.style.background = 'color-mix(in srgb, var(--ndr-accent) 16%, transparent)';
          } else {
            physBtn.innerHTML = '' + icons.atom + ' Physics: Off';
            physBtn.style.background = 'transparent';
          }
        });
      }

      // -------------------- Download SVG Button --------------------
      var svgBtn = document.createElement('button');
      svgBtn.id = 'svgButton';
      svgBtn.className = 'vis-button';
      svgBtn.innerHTML = '' + icons.fileCode + ' Download SVG';
      svgBtn.style.cssText = btnStyle;
      rightBar.appendChild(svgBtn);

      svgBtn.addEventListener('click', function() {
        var svgNS = 'http://www.w3.org/2000/svg';
        var positions = network.getPositions();
        var bodyNodes = network.body.nodes;
        var bodyEdges = network.body.edges;

        // --- collect rendered node properties from vis.js internals ---
        var nodeData = [];
        Object.keys(bodyNodes).forEach(function(id) {
          var bn = bodyNodes[id];
          if (!bn || !bn.options || !positions[id]) return;
          var opts = bn.options;
          var col = opts.color || {};
          nodeData.push({
            id: id,
            x: positions[id].x,
            y: positions[id].y,
            size: opts.size || 25,
            color: col.background || (typeof opts.color === 'string' ? opts.color : '#97C2FC'),
            borderColor: col.border || '#2B7CE9',
            borderWidth: opts.borderWidth || 1,
            label: opts.label || '',
            fontSize: (opts.font && opts.font.size) || 14,
            fontColor: (opts.font && opts.font.color) || '#343434'
          });
        });

        // --- build node color lookup for edge color inheritance ---
        var nodeColorMap = {};
        nodeData.forEach(function(n) { nodeColorMap[n.id] = n.color; });

        // --- collect rendered edge properties from vis.js internals ---
        var edgeData = [];
        Object.keys(bodyEdges).forEach(function(id) {
          var be = bodyEdges[id];
          if (!be || !be.options) return;
          var opts = be.options;
          var fromPos = positions[opts.from];
          var toPos = positions[opts.to];
          if (!fromPos || !toPos) return;

          // actual rendered width: vis.js stores it in options.width
          var width = opts.width || 1;

          // edge color: replicate vis.js color.inherit behavior
          // vis.js defaults to inherit:'from' (edge takes source node color)
          var color = '#848484';
          var inherit = (opts.color && opts.color.inherit !== undefined) ? opts.color.inherit : 'from';
          if (inherit === 'from' && nodeColorMap[opts.from]) {
            color = nodeColorMap[opts.from];
          } else if (inherit === 'to' && nodeColorMap[opts.to]) {
            color = nodeColorMap[opts.to];
          } else if (inherit === 'both' && nodeColorMap[opts.from]) {
            color = nodeColorMap[opts.from];
          } else if (opts.color) {
            if (typeof opts.color === 'string') { color = opts.color; }
            else if (opts.color.color) { color = opts.color.color; }
          }

          edgeData.push({
            fromX: fromPos.x, fromY: fromPos.y,
            toX: toPos.x, toY: toPos.y,
            width: width,
            color: color
          });
        });

        // --- compute bounding box including node radii and label space ---
        var pad = 80;
        var minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
        nodeData.forEach(function(n) {
          var rx = n.size + 10;
          var ry = n.size + n.fontSize + 10;
          if (n.x - rx < minX) minX = n.x - rx;
          if (n.y - ry < minY) minY = n.y - ry;
          if (n.x + rx > maxX) maxX = n.x + rx;
          if (n.y + ry > maxY) maxY = n.y + ry;
        });
        var vbX = minX - pad;
        var vbY = minY - pad;
        var vbW = (maxX - minX) + pad * 2;
        var vbH = (maxY - minY) + pad * 2;

        // SVG dimensions: maintain aspect ratio, target ~1200px wide
        var targetW = 1200;
        var scale = targetW / vbW;
        var targetH = Math.round(vbH * scale);

        var svg = document.createElementNS(svgNS, 'svg');
        svg.setAttribute('xmlns', svgNS);
        svg.setAttribute('width', targetW);
        svg.setAttribute('height', targetH);
        svg.setAttribute('viewBox', vbX + ' ' + vbY + ' ' + vbW + ' ' + vbH);
        svg.setAttribute('style', 'background:white;');

        // --- draw edges ---
        edgeData.forEach(function(e) {
          var line = document.createElementNS(svgNS, 'line');
          line.setAttribute('x1', e.fromX);
          line.setAttribute('y1', e.fromY);
          line.setAttribute('x2', e.toX);
          line.setAttribute('y2', e.toY);
          line.setAttribute('stroke', e.color);
          line.setAttribute('stroke-width', Math.max(e.width, 0.5));
          line.setAttribute('stroke-opacity', '0.6');
          svg.appendChild(line);
        });

        // --- draw nodes and labels ---
        nodeData.forEach(function(n) {
          var circle = document.createElementNS(svgNS, 'circle');
          circle.setAttribute('cx', n.x);
          circle.setAttribute('cy', n.y);
          circle.setAttribute('r', n.size);
          circle.setAttribute('fill', n.color);
          circle.setAttribute('stroke', n.borderColor);
          circle.setAttribute('stroke-width', n.borderWidth);
          svg.appendChild(circle);

          var text = document.createElementNS(svgNS, 'text');
          text.setAttribute('x', n.x);
          text.setAttribute('y', n.y + n.size + n.fontSize + 2);
          text.setAttribute('text-anchor', 'middle');
          text.setAttribute('font-size', n.fontSize);
          text.setAttribute('font-family', '-apple-system, BlinkMacSystemFont, sans-serif');
          text.setAttribute('fill', n.fontColor);
          text.textContent = n.label;
          svg.appendChild(text);
        });

        // --- serialize and download ---
        var serializer = new XMLSerializer();
        var source = serializer.serializeToString(svg);
        var blob = new Blob([source], {type:'image/svg+xml'});
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = downloadPrefix + '.svg';
        a.click();
      });

      // -------------------- Download PNG Button --------------------
      var pngBtn = document.createElement('button');
      pngBtn.id = 'pngButton';
      pngBtn.className = 'vis-button';
      pngBtn.innerHTML = '' + icons.image + ' Download PNG';
      pngBtn.style.cssText = btnStyle;
      rightBar.appendChild(pngBtn);

      pngBtn.addEventListener('click', function() {
        var canvas = network.canvas.frame.canvas;
        var dataURL = canvas.toDataURL('image/png');
        var a = document.createElement('a');
        a.href = dataURL;
        a.download = downloadPrefix + '.png';
        a.click();
      });

      // -------------------- Snapshot Helpers (always available) --------------------
      // Cache each node's color at first draw, before vis.js's highlightNearest
      // can ever mutate the DataSet entries (selection fades non-neighbors to
      // gray). We always serialize from this cache instead of reading the live
      // node state, so a save while a node is highlighted round-trips the
      // original palette. The cache is a deep clone so later vis.js mutations
      // cannot bleed in by reference.
      var originalNodeColors = {};
      (function snapshotOriginalColors() {
        try {
          network.body.data.nodes.get().forEach(function(n) {
            if (n.color !== undefined && n.color !== null) {
              originalNodeColors[n.id] = JSON.parse(JSON.stringify(n.color));
            }
          });
        } catch(e) {}
      })();

      // helper: snapshot full node state (positions + edits)
      function getNodeSnapshot() {
        var nodes = network.body.data.nodes.get();
        var positions = network.getPositions();
        return nodes.map(function(n) {
          var pos = positions[n.id];
          var snap = {
            id: n.id,
            label: n.label,
            x: Math.round(pos.x),
            y: Math.round(pos.y)
          };
          if (n.value !== undefined) snap.value = n.value;
          if (n.font && n.font.size) snap.fontSize = n.font.size;
          // Use the cached original color, never the (possibly faded) live one.
          if (originalNodeColors[n.id] !== undefined) snap.color = originalNodeColors[n.id];
          return snap;
        });
      }

      // helper: apply a snapshot to the network
      function applyNodeSnapshot(layout) {
        var updates = [];
        var lastFontSize = null;
        if (Array.isArray(layout)) {
          layout.forEach(function(item) {
            var update = { id: item.id, x: item.x, y: item.y };
            if (item.label !== undefined) update.label = item.label;
            if (item.value !== undefined) update.value = item.value;
            if (item.fontSize !== undefined) {
              update.font = { size: item.fontSize };
              lastFontSize = item.fontSize;
            }
            if (item.color !== undefined) update.color = item.color;
            updates.push(update);
          });
        } else {
          // object format {id: {x, y, ...}} from localStorage
          Object.keys(layout).forEach(function(id) {
            var item = layout[id];
            var update = { id: id, x: item.x, y: item.y };
            if (item.label !== undefined) update.label = item.label;
            if (item.value !== undefined) update.value = item.value;
            if (item.fontSize !== undefined) {
              update.font = { size: item.fontSize };
              lastFontSize = item.fontSize;
            }
            if (item.color !== undefined) update.color = item.color;
            updates.push(update);
          });
        }
        if (updates.length > 0) {
          network.body.data.nodes.update(updates);
        }
        // sync font size slider
        if (lastFontSize !== null) {
          slider.value = lastFontSize;
          span.innerHTML = lastFontSize;
        }
      }

      // -------------------- Save Layout Button (standalone only, hidden in report) --------------------
      if (!panelNs) {
      var saveBtn = document.createElement('button');
      saveBtn.id = 'saveLayoutButton';
      saveBtn.className = 'vis-button';
      saveBtn.innerHTML = '' + icons.save + ' Save Layout';
      saveBtn.style.position = 'fixed';
      saveBtn.style.top = '10px';
      saveBtn.style.right = '300px';
      saveBtn.style.zIndex = 9999;
      saveBtn.style.padding = '6px 10px';
      saveBtn.style.background = 'transparent';
      saveBtn.style.color = 'black';
      saveBtn.style.border = '1px solid rgb(204, 204, 204)';
      saveBtn.style.borderRadius = '6px';
      saveBtn.style.cursor = 'pointer';
      document.body.appendChild(saveBtn);

      saveBtn.addEventListener('click', function() {
        var saveData = {
          nodes: getNodeSnapshot(),
          keyLabels: keyData ? keyData.map(function(item) { return { color: item.color, label: item.label }; }) : null
        };
        var json = JSON.stringify(saveData, null, 2);
        var blob = new Blob([json], {type: 'application/json'});
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = downloadPrefix + '_layout.resondex_bn';
        a.click();
      });

      // -------------------- Load Layout Button --------------------
      var loadBtn = document.createElement('button');
      loadBtn.id = 'loadLayoutButton';
      loadBtn.className = 'vis-button';
      loadBtn.innerHTML = '' + icons.folderOpen + ' Load Layout';
      loadBtn.style.position = 'fixed';
      loadBtn.style.top = '10px';
      loadBtn.style.right = '440px';
      loadBtn.style.zIndex = 9999;
      loadBtn.style.padding = '6px 10px';
      loadBtn.style.background = 'transparent';
      loadBtn.style.color = 'black';
      loadBtn.style.border = '1px solid rgb(204, 204, 204)';
      loadBtn.style.borderRadius = '6px';
      loadBtn.style.cursor = 'pointer';
      document.body.appendChild(loadBtn);

      // hidden file input
      var fileInput = document.createElement('input');
      fileInput.type = 'file';
      fileInput.accept = '.resondex_bn,.json';
      fileInput.style.display = 'none';
      document.body.appendChild(fileInput);

      loadBtn.addEventListener('click', function() {
        fileInput.click();
      });

      fileInput.addEventListener('change', function() {
        var file = fileInput.files[0];
        if (!file) return;
        var reader = new FileReader();
        reader.onload = function(e) {
          try {
            var parsed = JSON.parse(e.target.result);
            // turn off physics and straighten edges to preserve loaded positions
            physicsEnabled = false;
            network.setOptions({ physics: { enabled: false }, edges: { smooth: false } });
            var physBtn = document.getElementById('physicsButton');
            if (physBtn) {
              physBtn.innerHTML = '' + icons.atom + ' Physics: Off';
              physBtn.style.background = 'transparent';
            }
            // handle new format (object with nodes + keyLabels) and old format (plain array)
            var layout = parsed.nodes || parsed;
            applyNodeSnapshot(layout);
            // restore legend labels
            if (parsed.keyLabels && keyData) {
              parsed.keyLabels.forEach(function(saved) {
                keyData.forEach(function(item) {
                  if (item.color === saved.color) item.label = saved.label;
                });
              });
              renderLegend();
            }
          } catch(err) {
            alert('Invalid layout file.');
          }
        };
        reader.readAsText(file);
        fileInput.value = '';
      });
      } // end if (!panelNs) — hide Save/Load buttons in report context

      // -------------------- localStorage Auto-save --------------------
      // Build a storage key from node ids to identify this specific network
      var nodeIds = network.body.data.nodes.getIds().sort().join(',');
      var storageKey = 'bn_layout_' + nodeIds.split('').reduce(function(h, c) {
        return ((h << 5) - h + c.charCodeAt(0)) | 0;
      }, 0);

      // Restore saved state from localStorage
      try {
        var saved = localStorage.getItem(storageKey);
        if (saved) {
          var parsed = JSON.parse(saved);
          var layout = parsed.nodes || parsed;
          applyNodeSnapshot(layout);
          if (parsed.keyLabels && keyData) {
            parsed.keyLabels.forEach(function(s) {
              keyData.forEach(function(item) {
                if (item.color === s.color) item.label = s.label;
              });
            });
            if (typeof renderLegend === 'function') renderLegend();
          }
        }
      } catch(e) {}

      // Auto-save full node state to localStorage after drag or edit
      function getFullSnapshot() {
        return {
          nodes: getNodeSnapshot(),
          keyLabels: keyData ? keyData.map(function(item) { return { color: item.color, label: item.label }; }) : null
        };
      }

      function saveToLocalStorage() {
        try {
          localStorage.setItem(storageKey, JSON.stringify(getFullSnapshot()));
        } catch(e) {}
      }

      // push snapshot to parent page (for report-level save/load)
      var suppressPush = false;
      function pushToParent() {
        if (!panelNs || suppressPush) return;
        try {
          window.parent.postMessage({ type: 'snapshotPush', nsKey: panelNs, data: getFullSnapshot() }, '*');
        } catch(e) {}
      }

      function saveState() {
        saveToLocalStorage();
        pushToParent();
      }

      network.on('dragEnd', saveState);

      // Hierarchical layout: vis.js keeps re-running its tier algorithm
      // on every nodes.update(), so drags get snapped back to algorithm
      // positions and saved snapshots can't restore custom positions on
      // load. After a short delay (long enough for the initial layout to
      // compute + stabilize), disable the hierarchical algorithm. The
      // initial tier-based visual is preserved as the starting positions;
      // from then on the nodes are free to drag and snapshot loads stick.
      if (layoutType === 'hierarchy') {
        setTimeout(function() {
          try {
            network.setOptions({ layout: { hierarchical: { enabled: false } } });
          } catch(e) {}
          // Push a fresh snapshot now that the algorithm is off — the
          // saved positions are the tier-computed ones and accurately
          // reflect what the user sees.
          pushToParent();
        }, 1500);
      }

      network.on('stabilized', function() {
        saveState();
        // if parent report requested physics off after stabilization, disable now
        if (window.__disablePhysicsAfterStabilize) {
          window.__disablePhysicsAfterStabilize = false;
          physicsEnabled = false;
          network.setOptions({ physics: { enabled: false } });
          var physBtn = document.getElementById('physicsButton');
          if (physBtn) {
            physBtn.innerHTML = '' + icons.atom + ' Physics: Off';
            physBtn.style.background = 'transparent';
          }
        }
      });
      network.body.data.nodes.on('update', saveState);

      // push initial state to parent (delayed to allow layout to settle)
      setTimeout(pushToParent, 500);

      // tell parent this iframe is ready (so parent can send pending load data)
      if (panelNs) {
        try {
          window.parent.postMessage({ type: 'iframeReady', nsKey: panelNs }, '*');
        } catch(e) {}
      }

      // -------------------- Right-click Edit Modal --------------------
      var modal = document.createElement('div');
      modal.id = 'editNodeModal';
      modal.style.cssText = 'display:none;position:fixed;top:0;left:0;width:100%;height:100%;z-index:99999;background:rgba(0,0,0,0.3);font-family:-apple-system,BlinkMacSystemFont,sans-serif;';
      var box = document.createElement('div');
      box.style.cssText = 'position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);background:#fff;border-radius:8px;padding:20px;min-width:280px;box-shadow:0 4px 16px rgba(0,0,0,0.2);';
      modal.appendChild(box);
      document.body.appendChild(modal);

      var editNodeId = null;

      function openEditModal(nodeId) {
        editNodeId = nodeId;
        var nd = network.body.data.nodes.get(nodeId);
        box.innerHTML = '<div style=\"font-size:16px;font-weight:600;margin-bottom:12px;\">Edit Node</div>'
          + '<label style=\"font-size:13px;color:#555;\">Label</label>'
          + '<input id=\"editLabel\" type=\"text\" value=\"' + (nd.label || '').replace(/\"/g,'&quot;') + '\" style=\"display:block;width:100%;padding:6px 8px;margin:4px 0 12px;border:1px solid #ccc;border-radius:4px;font-size:14px;box-sizing:border-box;\">'
          + '<label style=\"font-size:13px;color:#555;\">Value</label>'
          + '<input id=\"editValue\" type=\"number\" value=\"' + (nd.value !== undefined ? nd.value : '') + '\" style=\"display:block;width:100%;padding:6px 8px;margin:4px 0 16px;border:1px solid #ccc;border-radius:4px;font-size:14px;box-sizing:border-box;\">'
          + '<div style=\"text-align:right;\">'
          + '<button id=\"editCancel\" style=\"padding:6px 14px;margin-right:8px;border:1px solid #ccc;border-radius:4px;background:#fff;cursor:pointer;font-size:14px;\">Cancel</button>'
          + '<button id=\"editSave\" style=\"padding:6px 14px;border:none;border-radius:4px;background:#4A90D9;color:#fff;cursor:pointer;font-size:14px;\">Save</button>'
          + '</div>';
        modal.style.display = 'block';
        document.getElementById('editLabel').focus();

        document.getElementById('editCancel').onclick = function() { modal.style.display = 'none'; };
        box.addEventListener('keydown', function(e) { if (e.key === 'Enter') document.getElementById('editSave').click(); });
        document.getElementById('editSave').onclick = function() {
          var orig = network.body.data.nodes.get(editNodeId);
          var update = {
            id: editNodeId,
            label: document.getElementById('editLabel').value,
            group: orig.group,
            color: orig.color
          };
          var val = document.getElementById('editValue').value;
          if (val !== '') update.value = parseFloat(val);
          network.body.data.nodes.update(update);
          modal.style.display = 'none';
          // only sync to legend/attribute if this is a community tab (no keyData)
          if (!keyData) {
            try {
              var edits = {};
              edits[orig.color] = update.label;
              window.parent.postMessage({ type: 'nodeUpdate', edits: edits }, '*');
            } catch(e) {}
          } else {
            // attribute tab — notify parent of individual node label edit
            try {
              window.parent.postMessage({ type: 'nodeLabelUpdate', nodeId: editNodeId, label: update.label }, '*');
            } catch(e) {}
          }
        };
      }

      // close modal on background click
      modal.addEventListener('click', function(e) { if (e.target === modal) modal.style.display = 'none'; });

      network.on('oncontext', function(params) {
        var raw = params.event.srcEvent || params.event;
        raw.preventDefault();
        var nodeId = network.getNodeAt(params.pointer.DOM);
        if (!nodeId) return;
        openEditModal(nodeId);
      });

      // -------------------- Custom HTML Legend Overlay --------------------
      var keyData = ", ifelse(is.null(key_json), "null", as.character(key_json)), ";
      if (keyData) {
        var legend = document.createElement('div');
        legend.id = 'customLegend';
        // Fixed 200px width — matches the Select-by-ID button above
        // so the left-side control stack reads as a uniform column.
        legend.style.cssText = 'position:fixed;top:50px;left:10px;width:200px;max-height:70%;overflow-x:hidden;overflow-y:auto;background:var(--ndr-card-bg);color:var(--ndr-text);border:1px solid var(--ndr-border);border-radius:6px;padding:10px;font-family:-apple-system,BlinkMacSystemFont,sans-serif;font-size:13px;z-index:99999;pointer-events:auto;box-sizing:border-box;';
        document.body.appendChild(legend);
        legend.addEventListener('contextmenu', function(e) { e.preventDefault(); });

        function openKeyEditAll() {
          var html = '<div style=\"font-size:16px;font-weight:600;margin-bottom:12px;\">Edit Community Names</div>';
          keyData.forEach(function(item, i) {
            html += '<div style=\"display:flex;align-items:center;margin-bottom:8px;\">'
              + '<span style=\"display:inline-block;width:12px;height:12px;border-radius:50%;margin-right:8px;flex-shrink:0;background:' + item.color + ';\"></span>'
              + '<input id=\"keyLabel_' + i + '\" type=\"text\" value=\"' + (item.label || '').replace(/\"/g,'&quot;') + '\" style=\"flex:1;padding:6px 8px;border:1px solid #ccc;border-radius:4px;font-size:14px;box-sizing:border-box;\">'
              + '</div>';
          });
          html += '<div style=\"text-align:right;margin-top:8px;\">'
            + '<button id=\"editCancel\" style=\"padding:6px 14px;margin-right:8px;border:1px solid #ccc;border-radius:4px;background:#fff;cursor:pointer;font-size:14px;\">Cancel</button>'
            + '<button id=\"editSave\" style=\"padding:6px 14px;border:none;border-radius:4px;background:#4A90D9;color:#fff;cursor:pointer;font-size:14px;\">Save</button>'
            + '</div>';
          box.innerHTML = html;
          modal.style.display = 'block';
          document.getElementById('keyLabel_0').focus();
          document.getElementById('editCancel').onclick = function() { modal.style.display = 'none'; };
          box.addEventListener('keydown', function(ev) { if (ev.key === 'Enter') document.getElementById('editSave').click(); });
          document.getElementById('editSave').onclick = function() {
            keyData.forEach(function(item, i) {
              item.label = document.getElementById('keyLabel_' + i).value;
            });
            renderLegend();
            modal.style.display = 'none';
            // notify parent page so community tabs can sync
            try { window.parent.postMessage({ type: 'legendUpdate', keyData: keyData }, '*'); } catch(e) {}
            pushToParent();
          };
        }

        function renderLegend() {
          legend.innerHTML = '';
          keyData.forEach(function(item, idx) {
            var row = document.createElement('div');
            row.style.cssText = 'display:flex;align-items:flex-start;padding:4px 0;cursor:default;';
            var dot = document.createElement('span');
            dot.style.cssText = 'display:inline-block;width:12px;height:12px;border-radius:50%;margin-right:8px;margin-top:2px;flex-shrink:0;background:' + item.color + ';';
            var lbl = document.createElement('span');
            lbl.textContent = item.label;
            lbl.style.cssText = 'flex:1;word-wrap:break-word;overflow-wrap:break-word;';
            row.appendChild(dot);
            row.appendChild(lbl);
            legend.appendChild(row);

            row.style.cursor = 'pointer';
            row.addEventListener('dblclick', function(e) { e.preventDefault(); e.stopPropagation(); openKeyEditAll(); });
            row.addEventListener('mousedown', function(e) { if (e.button === 2) { e.preventDefault(); e.stopPropagation(); openKeyEditAll(); } });
          });
        }
        renderLegend();
      }

      // listen for messages from parent page
      window.addEventListener('message', function(evt) {
        if (!evt.data) return;

        // legend/node sync between tabs
        var edits = evt.data.edits || {};
        if (Object.keys(edits).length > 0) {
          if (evt.data.type === 'legendUpdate') {
            var nodes = network.body.data.nodes.get();
            var changed = [];
            nodes.forEach(function(n) {
              if (edits[n.color] && n.label !== edits[n.color]) {
                changed.push({ id: n.id, label: edits[n.color], group: n.group, color: n.color });
              }
            });
            if (changed.length > 0) network.body.data.nodes.update(changed);
          }
          if (evt.data.type === 'nodeUpdate' && keyData) {
            var redraw = false;
            keyData.forEach(function(item) {
              if (edits[item.color] && item.label !== edits[item.color]) {
                item.label = edits[item.color];
                redraw = true;
              }
            });
            if (redraw) renderLegend();
          }
        }

        // attribute node label edit forwarded from another accordion
        if (evt.data.type === 'nodeLabelUpdate' && evt.data.nodeId && evt.data.label) {
          var node = network.body.data.nodes.get(evt.data.nodeId);
          if (node && node.label !== evt.data.label) {
            network.body.data.nodes.update({ id: evt.data.nodeId, label: evt.data.label });
          }
        }

        // report-level load: parent sends snapshot data directly
        if (evt.data.type === 'applyReportLoad' && evt.data.snapshot) {
          window.bnApplySnapshot(evt.data.snapshot);
        }

        // parent tells us to re-fit (e.g. after becoming visible)
        if (evt.data.type === 'fitNetwork') {
          network.fit();
        }

        // parent broadcasts a dark/light mode change.
        //   Stage 1 (CSS): toggle data-bs-theme on the iframe's <html> →
        //     flips every brand-token-driven surface (legend, toolbar
        //     buttons, Select-by-ID, body bg) for free.
        //   Stage 2 (canvas): node labels are drawn by vis.js into the
        //     canvas (not CSS-reachable). Re-read the resolved --ndr-text
        //     token AFTER the attribute flip and push it via
        //     network.setOptions so labels stay legible on either bg.
        if (evt.data.type === 'setMode') {
          var mode = evt.data.mode === 'dark' ? 'dark' : 'light';
          document.documentElement.setAttribute('data-bs-theme', mode);
          if (typeof network !== 'undefined' && network) {
            try {
              var textColor = getComputedStyle(document.documentElement)
                .getPropertyValue('--ndr-text').trim() || (mode === 'dark' ? '#e8eaed' : '#212529');
              network.setOptions({
                nodes: { font: { color: textColor } }
              });
            } catch (err) {}
          }
        }

        // parent asks every iframe to deselect any active node before save,
        // so the saved visual state matches what loads back (no stuck
        // highlight/fade). Idempotent: harmless if nothing is selected.
        if (evt.data.type === 'deselectAll') {
          try { network.unselectAll(); } catch(e) {}
        }

        // consolidated edit sync from parent (on layout/tab switch)
        if (evt.data.type === 'syncEdits') {
          var legend = evt.data.legend || {};
          var nodeLabels = evt.data.nodeLabels || {};

          // apply community/legend edits
          if (Object.keys(legend).length > 0) {
            if (keyData) {
              // attribute view: update legend key labels only
              var redraw = false;
              keyData.forEach(function(item) {
                if (legend[item.color] && item.label !== legend[item.color]) {
                  item.label = legend[item.color];
                  redraw = true;
                }
              });
              if (redraw) renderLegend();
            } else {
              // community view: update node labels by color
              var nodes = network.body.data.nodes.get();
              var changed = [];
              nodes.forEach(function(n) {
                if (legend[n.color] && n.label !== legend[n.color]) {
                  changed.push({ id: n.id, label: legend[n.color], group: n.group, color: n.color });
                }
              });
              if (changed.length > 0) network.body.data.nodes.update(changed);
            }
          }

          // apply individual node label edits
          if (Object.keys(nodeLabels).length > 0) {
            var nlChanged = [];
            Object.keys(nodeLabels).forEach(function(nodeId) {
              var node = network.body.data.nodes.get(nodeId);
              if (node && node.label !== nodeLabels[nodeId]) {
                nlChanged.push({ id: nodeId, label: nodeLabels[nodeId] });
              }
            });
            if (nlChanged.length > 0) network.body.data.nodes.update(nlChanged);
          }

          // confirm to parent that edits are applied
          try {
            window.parent.postMessage({ type: 'editsSynced', nsKey: panelNs }, '*');
          } catch(e) {}
        }
      });

      // expose global functions for parent to call directly
      window.bnGetSnapshot = function() {
        return getFullSnapshot();
      };

      window.bnApplySnapshot = function(d) {
        if (!d) return;
        suppressPush = true;
        physicsEnabled = false;
        network.setOptions({ physics: { enabled: false }, edges: { smooth: false } });
        var physBtn = document.getElementById('physicsButton');
        if (physBtn) {
          physBtn.innerHTML = '' + icons.atom + ' Physics: Off';
          physBtn.style.background = 'transparent';
        }
        var layout = d.nodes || d;
        applyNodeSnapshot(layout);
        if (d.keyLabels && keyData) {
          d.keyLabels.forEach(function(s) {
            keyData.forEach(function(item) {
              if (item.color === s.color) item.label = s.label;
            });
          });
          if (typeof renderLegend === 'function') renderLegend();
        }
        suppressPush = false;
        pushToParent();
      };


    }
  ")
    )

}


