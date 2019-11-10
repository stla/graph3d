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

//        graph3d.setOptions(x.options2);

      },

//      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

//      }

      s: graph3d
    };
  }
});
