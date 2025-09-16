#' bn_visNetwork_deliverable_interactivity
#' @description bn_visNetwork_deliverable_interactivity
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
      nodesIdSelection = TRUE,
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

      // Toggle slider visibility dynamically under the button
      btn.addEventListener('click', function(e){
        if(sliderContainer.style.display === 'none') {
          // Get button position
          var rect = btn.getBoundingClientRect();
          sliderContainer.style.top = (rect.bottom + 6) + 'px'; // 6px margin
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

    }
  "
    )

}


