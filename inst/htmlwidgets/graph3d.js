HTMLWidgets.widget({

  name: 'graph3d',

  type: 'output',

  factory: function(el, width, height) {

    var graph3d;

    return {

      renderValue: function(x) {

        var data = new vis.DataSet();
        data.add(HTMLWidgets.dataframeToD3(x.data));

        graph3d = new vis.Graph3d(el, data, x.options);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size
        // maybe:
        // graph3d.setOptions({width: width, height: height});
        // or:
        graph3d.setSize(width, height);

      },

      s: graph3d
    };
  }
});
