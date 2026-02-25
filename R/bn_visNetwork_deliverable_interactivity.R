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

      // Load Font Awesome
      var link = document.createElement('link');
      link.rel = 'stylesheet';
      link.href = 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css';
      document.head.appendChild(link);

      // shared button dimensions
      var btnW = 130;
      var btnH = 34;
      var btnGap = 6;
      var btnStyle = 'width:' + btnW + 'px;height:' + btnH + 'px;padding:0 10px;background:transparent;color:black;border:1px solid rgb(204,204,204);border-radius:6px;cursor:pointer;font-size:13px;box-sizing:border-box;display:flex;align-items:center;justify-content:center;gap:4px;flex-shrink:0;white-space:nowrap;';

      // right-side button container (prevents overlap with left-side controls)
      var rightBar = document.createElement('div');
      rightBar.id = 'rightButtonBar';
      rightBar.style.cssText = 'position:fixed;top:10px;right:10px;left:220px;z-index:9999;display:flex;gap:' + btnGap + 'px;justify-content:flex-end;flex-wrap:wrap;pointer-events:none;';
      document.body.appendChild(rightBar);

      // style the nodesIdSelection select to match button dimensions
      var selStyle = document.createElement('style');
      selStyle.textContent = '.vis-configuration-wrapper select, div[style*=\"nodesIdSelection\"] select, .vis-network select { width:' + btnW + 'px !important; height:' + btnH + 'px !important; padding:0 10px !important; font-size:13px !important; border:1px solid rgb(204,204,204) !important; border-radius:6px !important; box-sizing:border-box !important; max-height:300px !important; } #rightButtonBar > button { pointer-events:auto; } .nav-ctrl:hover{background:#f0f0f0!important;border-color:#999!important;} .nav-ctrl:active{background:#e0e0e0!important;}';
      document.head.appendChild(selStyle);

      // -------------------- Custom Navigation Controls --------------------
      var navSize = 30;
      var navGap = 4;
      var navBtnStyle = 'width:' + navSize + 'px;height:' + navSize + 'px;border-radius:50%;border:1px solid #ccc;background:white;color:black;cursor:pointer;display:flex;align-items:center;justify-content:center;font-size:12px;padding:0;';
      function makeNavBtn(icon) {
        var b = document.createElement('button');
        b.className = 'nav-ctrl';
        b.innerHTML = '<i class=\"fa ' + icon + '\"></i>';
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

      var upBtn = makeNavBtn('fa-chevron-up');
      upBtn.addEventListener('click', function() { panView(0, -120); });
      navRow1.appendChild(upBtn);
      var leftBtn = makeNavBtn('fa-chevron-left');
      leftBtn.addEventListener('click', function() { panView(-120, 0); });
      navRow2.appendChild(leftBtn);
      var downBtn = makeNavBtn('fa-chevron-down');
      downBtn.addEventListener('click', function() { panView(0, 120); });
      navRow2.appendChild(downBtn);
      var rightBtn = makeNavBtn('fa-chevron-right');
      rightBtn.addEventListener('click', function() { panView(120, 0); });
      navRow2.appendChild(rightBtn);

      navPad.appendChild(navRow1);
      navPad.appendChild(navRow2);
      document.body.appendChild(navPad);

      // zoom controls (bottom-right)
      var zoomPad = document.createElement('div');
      zoomPad.style.cssText = 'position:fixed;bottom:10px;right:10px;z-index:9999;display:flex;gap:' + navGap + 'px;';
      var zoomOutBtn = makeNavBtn('fa-minus');
      zoomOutBtn.addEventListener('click', function() {
        network.moveTo({ scale: network.getScale() * 0.7, animation: { duration: 200 } });
      });
      zoomPad.appendChild(zoomOutBtn);
      var zoomInBtn = makeNavBtn('fa-plus');
      zoomInBtn.addEventListener('click', function() {
        network.moveTo({ scale: network.getScale() * 1.4, animation: { duration: 200 } });
      });
      zoomPad.appendChild(zoomInBtn);
      var fitBtn = makeNavBtn('fa-expand');
      fitBtn.addEventListener('click', function() { network.fit({ animation: { duration: 300 } }); });
      zoomPad.appendChild(fitBtn);
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
        var ddBtn = document.createElement('button');
        ddBtn.id = 'idSelectBtn';
        ddBtn.style.cssText = 'width:' + btnW + 'px;height:' + btnH + 'px;padding:0 10px;background:transparent;color:black;border:1px solid rgb(204,204,204);border-radius:6px;cursor:pointer;font-size:13px;box-sizing:border-box;text-align:left;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;';
        ddBtn.textContent = 'Select by ID';
        ddWrap.appendChild(ddBtn);

        var ddPanel = document.createElement('div');
        ddPanel.style.cssText = 'display:none;position:absolute;top:' + (btnH + 4) + 'px;left:0;min-width:' + btnW + 'px;max-height:300px;overflow-y:auto;background:white;border:1px solid #ccc;border-radius:6px;box-shadow:0 2px 8px rgba(0,0,0,0.15);padding:4px 0;';

        nodeList.forEach(function(nd) {
          var item = document.createElement('div');
          item.textContent = nd.label || nd.id;
          item.style.cssText = 'padding:6px 10px;cursor:pointer;font-size:13px;white-space:nowrap;';
          item.addEventListener('mouseenter', function() { item.style.background = '#f0f0f0'; });
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
      btn.innerHTML = '<i class=\"fa fa-pencil-alt\"></i> Font Size';
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
      slider.value = 20;
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
      nodesData.forEach(function(n){ if(!n.font) n.font={size:20}; });
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
          ? '<i class=\"fa fa-atom\"></i> Physics: On'
          : '<i class=\"fa fa-atom\"></i> Physics: Off';
        physBtn.style.cssText = btnStyle + (physicsEnabled ? 'background:rgba(200,230,255,0.3);' : '');
        rightBar.appendChild(physBtn);

        physBtn.addEventListener('click', function() {
          physicsEnabled = !physicsEnabled;
          network.setOptions({ physics: { enabled: physicsEnabled } });
          if (physicsEnabled) {
            physBtn.innerHTML = '<i class=\"fa fa-atom\"></i> Physics: On';
            physBtn.style.background = 'rgba(200,230,255,0.3)';
          } else {
            physBtn.innerHTML = '<i class=\"fa fa-atom\"></i> Physics: Off';
            physBtn.style.background = 'transparent';
          }
        });
      }

      // -------------------- Download SVG Button --------------------
      var svgBtn = document.createElement('button');
      svgBtn.id = 'svgButton';
      svgBtn.className = 'vis-button';
      svgBtn.innerHTML = '<i class=\"fa fa-file-code\"></i> Download SVG';
      svgBtn.style.cssText = btnStyle;
      rightBar.appendChild(svgBtn);

      svgBtn.addEventListener('click', function() {
        var svgNS = 'http://www.w3.org/2000/svg';
        var container = network.body.container;
        var width = container.clientWidth;
        var height = container.clientHeight;
        var svg = document.createElementNS(svgNS, 'svg');
        svg.setAttribute('width', width);
        svg.setAttribute('height', height);

        // Edges
        var edges = network.body.data.edges.get();
        edges.forEach(function(e){
          var from = network.getPositions([e.from])[e.from];
          var to = network.getPositions([e.to])[e.to];
          var line = document.createElementNS(svgNS, 'line');
          line.setAttribute('x1', from.x);
          line.setAttribute('y1', from.y);
          line.setAttribute('x2', to.x);
          line.setAttribute('y2', to.y);
          line.setAttribute('stroke', '#848484');
          line.setAttribute('stroke-width', 2);
          svg.appendChild(line);
        });

        // Nodes and labels
        var nodes = network.body.data.nodes.get();
        nodes.forEach(function(n){
          var pos = network.getPositions([n.id])[n.id];
          var circle = document.createElementNS(svgNS, 'circle');
          circle.setAttribute('cx', pos.x);
          circle.setAttribute('cy', pos.y);
          circle.setAttribute('r', 20);
          circle.setAttribute('fill', '#97C2FC');
          circle.setAttribute('stroke', '#2B7CE9');
          svg.appendChild(circle);

          var text = document.createElementNS(svgNS, 'text');
          text.setAttribute('x', pos.x);
          text.setAttribute('y', pos.y + 5);
          text.setAttribute('text-anchor', 'middle');
          text.setAttribute('font-size', n.font ? n.font.size : 20);
          text.setAttribute('fill', 'black');
          text.textContent = n.label;
          svg.appendChild(text);
        });

        // Serialize and download
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
      pngBtn.innerHTML = '<i class=\"fa fa-image\"></i> Download PNG';
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
          if (n.color) snap.color = n.color;
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
      saveBtn.innerHTML = '<i class=\"fa fa-floppy-disk\"></i> Save Layout';
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
      loadBtn.innerHTML = '<i class=\"fa fa-folder-open\"></i> Load Layout';
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
              physBtn.innerHTML = '<i class=\"fa fa-atom\"></i> Physics: Off';
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
      network.on('stabilized', saveState);
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
        legend.style.cssText = 'position:fixed;top:50px;left:10px;min-width:' + btnW + 'px;max-width:200px;max-height:70%;overflow-x:hidden;overflow-y:auto;background:rgba(255,255,255,0.95);border:1px solid #ccc;border-radius:6px;padding:10px;font-family:-apple-system,BlinkMacSystemFont,sans-serif;font-size:13px;z-index:99999;pointer-events:auto;box-sizing:border-box;';
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

        // report-level load: parent sends snapshot data directly
        if (evt.data.type === 'applyReportLoad' && evt.data.snapshot) {
          window.bnApplySnapshot(evt.data.snapshot);
        }

        // parent tells us to re-fit (e.g. after becoming visible)
        if (evt.data.type === 'fitNetwork') {
          network.fit();
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
          physBtn.innerHTML = '<i class=\"fa fa-atom\"></i> Physics: Off';
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


