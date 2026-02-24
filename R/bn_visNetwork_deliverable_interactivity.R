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
bn_visNetwork_deliverable_interactivity <- function(obj){

  obj %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,       # allow moving nodes
      dragView  = TRUE,       # allow moving the canvas
      zoomView  = TRUE,       # allow zoom
      multiselect = TRUE,      # allow multiple selection
      navigationButtons = TRUE
    ) %>%
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = list(
        enabled = TRUE,
        style = "margin: 10px; padding: 6px 10px; border: 1px solid rgb(204, 204, 204); border-radius: 6px; font-size: 14px; background: transparent; cursor: pointer; max-width: 200px;"
      ),
      manipulation = list(
        enabled = TRUE,
        addNode = FALSE,
        addEdge = FALSE,
        deleteNode = FALSE,
        deleteEdge = FALSE,
        editEdge = FALSE,
        editNode = TRUE,
        editNodeCols = list(
          "text" = c("id", "label"),
          "number" = c("value")
        )
      )
    ) %>%
    visNetwork::visEvents(
      afterDrawing = "
    function() {
      if(document.getElementById('fontButton')) return;

      var network = this;

      // Load Font Awesome
      var link = document.createElement('link');
      link.rel = 'stylesheet';
      link.href = 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css';
      document.head.appendChild(link);

      // -------------------- Font Size Button --------------------
      var btn = document.createElement('button');
      btn.id = 'fontButton';
      btn.className = 'vis-button';
      btn.innerHTML = '<i class=\"fa fa-pencil-alt\"></i> Font Size';
      btn.style.position = 'fixed';
      btn.style.top = '10px';
      btn.style.left = '40px';
      btn.style.zIndex = 9999;
      btn.style.padding = '6px 10px';
      btn.style.marginBottom = '6px';
      btn.style.background = 'transparent';
      btn.style.color = 'black';
      btn.style.border = '1px solid rgb(204, 204, 204)';
      btn.style.borderRadius = '6px';
      btn.style.cursor = 'pointer';
      document.body.appendChild(btn);

      // Slider container (hidden initially)
      var sliderContainer = document.createElement('div');
      sliderContainer.style.position = 'fixed';
      sliderContainer.style.top = '50px';
      sliderContainer.style.left = '40px';
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

      // Toggle slider visibility dynamically above the button
      btn.addEventListener('click', function(e){
        if(sliderContainer.style.display === 'none') {
          // Get button position
          var rect = btn.getBoundingClientRect();
          sliderContainer.style.top = (rect.top - sliderContainer.offsetHeight - 6) + 'px';
          sliderContainer.style.left = rect.left + 'px';
          sliderContainer.style.display = 'block';
          // reposition after display so offsetHeight is accurate
          var sliderRect = sliderContainer.getBoundingClientRect();
          sliderContainer.style.top = (rect.top - sliderRect.height - 6) + 'px';
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

      // -------------------- Download PNG Button --------------------
      var pngBtn = document.createElement('button');
      pngBtn.id = 'pngButton';
      pngBtn.className = 'vis-button';
      pngBtn.innerHTML = '<i class=\"fa fa-image\"></i> Download PNG';
      pngBtn.style.position = 'fixed';
      pngBtn.style.top = '10px';
      pngBtn.style.right = '10px';
      pngBtn.style.zIndex = 9999;
      pngBtn.style.padding = '6px 10px';
      pngBtn.style.background = 'transparent';
      pngBtn.style.color = 'black';
      pngBtn.style.border = '1px solid rgb(204, 204, 204)';
      pngBtn.style.borderRadius = '6px';
      pngBtn.style.cursor = 'pointer';
      document.body.appendChild(pngBtn);

      pngBtn.addEventListener('click', function() {
        var canvas = network.canvas.frame.canvas;
        var dataURL = canvas.toDataURL('image/png');
        var a = document.createElement('a');
        a.href = dataURL;
        a.download = 'network.png';
        a.click();
      });

      // -------------------- Download SVG Button --------------------
      var svgBtn = document.createElement('button');
      svgBtn.id = 'svgButton';
      svgBtn.className = 'vis-button';
      svgBtn.innerHTML = '<i class=\"fa fa-file-code\"></i> Download SVG';
      svgBtn.style.position = 'fixed';
      svgBtn.style.top = '10px';
      svgBtn.style.right = '150px';
      svgBtn.style.zIndex = 9999;
      svgBtn.style.padding = '6px 10px';
      svgBtn.style.background = 'transparent';
      svgBtn.style.color = 'black';
      svgBtn.style.border = '1px solid rgb(204, 204, 204)';
      svgBtn.style.borderRadius = '6px';
      svgBtn.style.cursor = 'pointer';
      document.body.appendChild(svgBtn);

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
        a.download = 'network.svg';
        a.click();
      });

      // -------------------- Save Layout Button --------------------
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
          network.fit();
        }
        // sync font size slider
        if (lastFontSize !== null) {
          slider.value = lastFontSize;
          span.innerHTML = lastFontSize;
        }
      }

      saveBtn.addEventListener('click', function() {
        var json = JSON.stringify(getNodeSnapshot(), null, 2);
        var blob = new Blob([json], {type: 'application/json'});
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = 'network_layout.json';
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
      fileInput.accept = '.json';
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
            var layout = JSON.parse(e.target.result);
            applyNodeSnapshot(layout);
          } catch(err) {
            alert('Invalid layout file.');
          }
        };
        reader.readAsText(file);
        fileInput.value = '';
      });

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
          applyNodeSnapshot(JSON.parse(saved));
        }
      } catch(e) {}

      // Auto-save full node state to localStorage after drag or edit
      function saveToLocalStorage() {
        try {
          localStorage.setItem(storageKey, JSON.stringify(getNodeSnapshot()));
        } catch(e) {}
      }
      network.on('dragEnd', saveToLocalStorage);
      network.body.data.nodes.on('update', saveToLocalStorage);

    }
  "
    )

}


