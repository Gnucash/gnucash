
SET(gncjqplot_DATA
  jquery.min.js
  jquery.jqplot.js
  jquery.jqplot.css
  plugins/jqplot.barRenderer.js
  plugins/jqplot.BezierCurveRenderer.js
  plugins/jqplot.blockRenderer.js
  plugins/jqplot.bubbleRenderer.js
  plugins/jqplot.canvasAxisLabelRenderer.js
  plugins/jqplot.canvasAxisTickRenderer.js
  plugins/jqplot.canvasTextRenderer.js
  plugins/jqplot.categoryAxisRenderer.js
  plugins/jqplot.ciParser.js
  plugins/jqplot.cursor.js
  plugins/jqplot.dateAxisRenderer.js
  plugins/jqplot.donutRenderer.js
  plugins/jqplot.dragable.js
  plugins/jqplot.enhancedLegendRenderer.js
  plugins/jqplot.funnelRenderer.js
  plugins/jqplot.highlighter.js
  plugins/jqplot.json2.js
  plugins/jqplot.logAxisRenderer.js
  plugins/jqplot.mekkoAxisRenderer.js
  plugins/jqplot.mekkoRenderer.js
  plugins/jqplot.meterGaugeRenderer.js
  plugins/jqplot.ohlcRenderer.js
  plugins/jqplot.pieRenderer.js
  plugins/jqplot.pointLabels.js
  plugins/jqplot.trendline.js
)

INSTALL(FILES ${gncjqplot_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/jqplot)

SET_DIST_LIST(jqplot_DIST CMakeLists.txt jquery.js ${gncjqplot_DATA})
