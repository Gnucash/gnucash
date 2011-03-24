/**
 * Title: jqPlot Charts
 * 
 * Pure JavaScript plotting plugin for jQuery.
 * 
 * About: Version
 * 
 * 0.9.7r635 
 * 
 * About: Copyright & License
 * 
 * Copyright (c) 2009 - 2010 Chris Leonello
 * jqPlot is currently available for use in all personal or commercial projects 
 * under both the MIT and GPL version 2.0 licenses. This means that you can 
 * choose the license that best suits your project and use it accordingly.
 * 
 * See <GPL Version 2> and <MIT License> contained within this distribution for further information. 
 *
 * The author would appreciate an email letting him know of any substantial
 * use of jqPlot.  You can reach the author at: chris at jqplot dot com 
 * or see http://www.jqplot.com/info.php.  This is, of course, not required.
 *
 * If you are feeling kind and generous, consider supporting the project by
 * making a donation at: http://www.jqplot.com/donate.php.
 * 
 * jqPlot includes `date instance methods and printf/sprintf functions by other authors:
 * 
 * Date instance methods:
 *
 *     author Ken Snyder (ken d snyder at gmail dot com)
 *     date 2008-09-10
 *     version 2.0.2 (http://kendsnyder.com/sandbox/date/)     
 *     license Creative Commons Attribution License 3.0 (http://creativecommons.org/licenses/by/3.0/)
 *
 * JavaScript printf/sprintf functions:
 *
 *     version 2007.04.27
 *     author Ash Searle
 *     http://hexmen.com/blog/2007/03/printf-sprintf/
 *     http://hexmen.com/js/sprintf.js
 *     The author (Ash Searle) has placed this code in the public domain:
 *     "This code is unrestricted: you are free to use it however you like."
 * 
 * 
 * About: Introduction
 * 
 * jqPlot requires jQuery (1.4+ required for certain features). jQuery 1.4.1 is included in the distribution.  
 * To use jqPlot include jQuery, the jqPlot jQuery plugin, the jqPlot css file and optionally 
 * the excanvas script for IE support in your web page:
 * 
 * > <!--[if IE]><script language="javascript" type="text/javascript" src="excanvas.js"></script><![endif]-->
 * > <script language="javascript" type="text/javascript" src="jquery-1.4.2.min.js"></script>
 * > <script language="javascript" type="text/javascript" src="jquery.jqplot.min.js"></script>
 * > <link rel="stylesheet" type="text/css" href="jquery.jqplot.css" />
 * 
 * jqPlot can be customized by overriding the defaults of any of the objects which make
 * up the plot. The general usage of jqplot is:
 * 
 * > chart = $.jqplot('targetElemId', [dataArray,...], {optionsObject});
 * 
 * The options available to jqplot are detailed in <jqPlot Options> in the jqPlotOptions.txt file.
 * 
 * An actual call to $.jqplot() may look like the 
 * examples below:
 * 
 * > chart = $.jqplot('chartdiv',  [[[1, 2],[3,5.12],[5,13.1],[7,33.6],[9,85.9],[11,219.9]]]);
 * 
 * or
 * 
 * > dataArray = [34,12,43,55,77];
 * > chart = $.jqplot('targetElemId', [dataArray, ...], {title:'My Plot', axes:{yaxis:{min:20, max:100}}});
 * 
 * For more inforrmation, see <jqPlot Usage>.
 * 
 * About: Usage
 * 
 * See <jqPlot Usage>
 * 
 * About: Available Options 
 * 
 * See <jqPlot Options> for a list of options available thorugh the options object (not complete yet!)
 * 
 * About: Options Usage
 * 
 * See <Options Tutorial>
 * 
 * About: Changes
 * 
 * See <Change Log>
 * 
 */

(function($) {
    // make sure undefined is undefined
    var undefined;

    /**
     * Class: $.jqplot
     * jQuery function called by the user to create a plot.
     *  
     * Parameters:
     * target - ID of target element to render the plot into.
     * data - an array of data series.
     * options - user defined options object.  See the individual classes for available options.
     * 
     * Properties:
     * config - object to hold configuration information for jqPlot plot object.
     * 
     * attributes:
     * enablePlugins - False to disable plugins by default.  Plugins must then be explicitly 
     *   enabled in the individual plot options.  Default: false.
     *   This property sets the "show" property of certain plugins to true or false.
     *   Only plugins that can be immediately active upon loading are affected.  This includes
     *   non-renderer plugins like cursor, dragable, highlighter, and trendline.
     * defaultHeight - Default height for plots where no css height specification exists.  This
     *   is a jqplot wide default.
     * defaultWidth - Default height for plots where no css height specification exists.  This
     *   is a jqplot wide default.
     */

    $.jqplot = function(target, data, options) {
        var _data, _options;
        
        if (options == null) {
            if (data instanceof Array) {
                _data = data;
                _options = null;   
            }
            
            else if (data.constructor == Object) {
                _data = null;
                _options = data;
            }
        }
        else {
            _data = data;
            _options = options;
        }
        var plot = new jqPlot();
        // remove any error class that may be stuck on target.
        $('#'+target).removeClass('jqplot-error');
        
        if ($.jqplot.config.catchErrors) {
            try {
                plot.init(target, _data, _options);
                plot.draw();
                plot.themeEngine.init.call(plot);
                return plot;
            }
            catch(e) {
                var msg = $.jqplot.config.errorMessage || e.message;
                $('#'+target).append('<div class="jqplot-error-message">'+msg+'</div>');
                $('#'+target).addClass('jqplot-error');
                document.getElementById(target).style.background = $.jqplot.config.errorBackground;
                document.getElementById(target).style.border = $.jqplot.config.errorBorder;
                document.getElementById(target).style.fontFamily = $.jqplot.config.errorFontFamily;
                document.getElementById(target).style.fontSize = $.jqplot.config.errorFontSize;
                document.getElementById(target).style.fontStyle = $.jqplot.config.errorFontStyle;
                document.getElementById(target).style.fontWeight = $.jqplot.config.errorFontWeight;
            }
        }
        else {        
            plot.init(target, _data, _options);
            plot.draw();
            plot.themeEngine.init.call(plot);
            return plot;
        }
    };
        
    $.jqplot.debug = 1;
    $.jqplot.config = {
        debug:1,
        enablePlugins:false,
        defaultHeight:300,
        defaultWidth:400,
        UTCAdjust:false,
        timezoneOffset: new Date(new Date().getTimezoneOffset() * 60000),
        errorMessage: '',
        errorBackground: '',
        errorBorder: '',
        errorFontFamily: '',
        errorFontSize: '',
        errorFontStyle: '',
        errorFontWeight: '',
        catchErrors: false,
        defaultTickFormatString: "%.1f"
    };
    
    $.jqplot.enablePlugins = $.jqplot.config.enablePlugins;
    
    // canvas related tests taken from modernizer:
    // Copyright © 2009–2010 Faruk Ates.
    // http://www.modernizr.com
    
    $.jqplot.support_canvas = function() {
        return !!document.createElement('canvas').getContext;
    };
            
    $.jqplot.support_canvas_text = function() {
        return !!(document.createElement('canvas').getContext && typeof document.createElement('canvas').getContext('2d').fillText == 'function');
    };
    
    $.jqplot.use_excanvas = ($.browser.msie && !$.jqplot.support_canvas()) ? true : false;
    
    /**
     * 
     * Hooks: jqPlot Pugin Hooks
     * 
     * $.jqplot.preInitHooks - called before initialization.
     * $.jqplot.postInitHooks - called after initialization.
     * $.jqplot.preParseOptionsHooks - called before user options are parsed.
     * $.jqplot.postParseOptionsHooks - called after user options are parsed.
     * $.jqplot.preDrawHooks - called before plot draw.
     * $.jqplot.postDrawHooks - called after plot draw.
     * $.jqplot.preDrawSeriesHooks - called before each series is drawn.
     * $.jqplot.postDrawSeriesHooks - called after each series is drawn.
     * $.jqplot.preDrawLegendHooks - called before the legend is drawn.
     * $.jqplot.addLegendRowHooks - called at the end of legend draw, so plugins
     *     can add rows to the legend table.
     * $.jqplot.preSeriesInitHooks - called before series is initialized.
     * $.jqplot.postSeriesInitHooks - called after series is initialized.
     * $.jqplot.preParseSeriesOptionsHooks - called before series related options
     *     are parsed.
     * $.jqplot.postParseSeriesOptionsHooks - called after series related options
     *     are parsed.
     * $.jqplot.eventListenerHooks - called at the end of plot drawing, binds
     *     listeners to the event canvas which lays on top of the grid area.
     * $.jqplot.preDrawSeriesShadowHooks - called before series shadows are drawn.
     * $.jqplot.postDrawSeriesShadowHooks - called after series shadows are drawn.
     * 
     */
    
    $.jqplot.preInitHooks = [];
    $.jqplot.postInitHooks = [];
    $.jqplot.preParseOptionsHooks = [];
    $.jqplot.postParseOptionsHooks = [];
    $.jqplot.preDrawHooks = [];
    $.jqplot.postDrawHooks = [];
    $.jqplot.preDrawSeriesHooks = [];
    $.jqplot.postDrawSeriesHooks = [];
    $.jqplot.preDrawLegendHooks = [];
    $.jqplot.addLegendRowHooks = [];
    $.jqplot.preSeriesInitHooks = [];
    $.jqplot.postSeriesInitHooks = [];
    $.jqplot.preParseSeriesOptionsHooks = [];
    $.jqplot.postParseSeriesOptionsHooks = [];
    $.jqplot.eventListenerHooks = [];
    $.jqplot.preDrawSeriesShadowHooks = [];
    $.jqplot.postDrawSeriesShadowHooks = [];

    // A superclass holding some common properties and methods.
    $.jqplot.ElemContainer = function() {
        this._elem;
        this._plotWidth;
        this._plotHeight;
        this._plotDimensions = {height:null, width:null};
    };
    
    $.jqplot.ElemContainer.prototype.createElement = function(el, offsets, clss, cssopts, attrib) {
        this._offsets = offsets;
        var klass = clss || 'jqplot';
        var elem = document.createElement(el);
        this._elem = $(elem);
        this._elem.addClass(klass);
        this._elem.css(cssopts);
        this._elem.attr(attrib);
        return this._elem;
    };
    
    $.jqplot.ElemContainer.prototype.getWidth = function() {
        if (this._elem) {
            return this._elem.outerWidth(true);
        }
        else {
            return null;
        }
    };
    
    $.jqplot.ElemContainer.prototype.getHeight = function() {
        if (this._elem) {
            return this._elem.outerHeight(true);
        }
        else {
            return null;
        }
    };
    
    $.jqplot.ElemContainer.prototype.getPosition = function() {
        if (this._elem) {
            return this._elem.position();
        }
        else {
            return {top:null, left:null, bottom:null, right:null};
        }
    };
    
    $.jqplot.ElemContainer.prototype.getTop = function() {
        return this.getPosition().top;
    };
    
    $.jqplot.ElemContainer.prototype.getLeft = function() {
        return this.getPosition().left;
    };
    
    $.jqplot.ElemContainer.prototype.getBottom = function() {
        return this._elem.css('bottom');
    };
    
    $.jqplot.ElemContainer.prototype.getRight = function() {
        return this._elem.css('right');
    };
    

    /**
     * Class: Axis
     * An individual axis object.  Cannot be instantiated directly, but created
     * by the Plot oject.  Axis properties can be set or overriden by the 
     * options passed in from the user.
     * 
     */
    function Axis(name) {
        $.jqplot.ElemContainer.call(this);
        // Group: Properties
        //
        // Axes options are specified within an axes object at the top level of the 
        // plot options like so:
        // > {
        // >    axes: {
        // >        xaxis: {min: 5},
        // >        yaxis: {min: 2, max: 8, numberTicks:4},
        // >        x2axis: {pad: 1.5},
        // >        y2axis: {ticks:[22, 44, 66, 88]}
        // >        }
        // > }
        // There are 4 axes, 'xaxis', 'yaxis', 'x2axis', 'y2axis'.  Any or all of 
        // which may be specified.
        this.name = name;
        this._series = [];
        // prop: show
        // Wether to display the axis on the graph.
        this.show = false;
        // prop: tickRenderer
        // A class of a rendering engine for creating the ticks labels displayed on the plot, 
        // See <$.jqplot.AxisTickRenderer>.
        this.tickRenderer = $.jqplot.AxisTickRenderer;
        // prop: tickOptions
        // Options that will be passed to the tickRenderer, see <$.jqplot.AxisTickRenderer> options.
        this.tickOptions = {};
        // prop: labelRenderer
        // A class of a rendering engine for creating an axis label.
        this.labelRenderer = $.jqplot.AxisLabelRenderer;
        // prop: labelOptions
        // Options passed to the label renderer.
        this.labelOptions = {};
        // prop: label
        // Label for the axis
        this.label = null;
        // prop: showLabel
        // true to show the axis label.
        this.showLabel = true;
        // prop: min
        // minimum value of the axis (in data units, not pixels).
        this.min=null;
        // prop: max
        // maximum value of the axis (in data units, not pixels).
        this.max=null;
        // prop: autoscale
        // Autoscale the axis min and max values to provide sensible tick spacing.
        // If axis min or max are set, autoscale will be turned off.
        // The numberTicks, tickInterval and pad options do work with 
        // autoscale, although tickInterval has not been tested yet.
        // padMin and padMax do nothing when autoscale is on.
        this.autoscale = false;
        // prop: pad
        // Padding to extend the range above and below the data bounds.
        // The data range is multiplied by this factor to determine minimum and maximum axis bounds.
        // A value of 0 will be interpreted to mean no padding, and pad will be set to 1.0.
        this.pad = 1.2;
        // prop: padMax
        // Padding to extend the range above data bounds.
        // The top of the data range is multiplied by this factor to determine maximum axis bounds.
        // A value of 0 will be interpreted to mean no padding, and padMax will be set to 1.0.
        this.padMax = null;
        // prop: padMin
        // Padding to extend the range below data bounds.
        // The bottom of the data range is multiplied by this factor to determine minimum axis bounds.
        // A value of 0 will be interpreted to mean no padding, and padMin will be set to 1.0.
        this.padMin = null;
        // prop: ticks
        // 1D [val, val, ...] or 2D [[val, label], [val, label], ...] array of ticks for the axis.
        // If no label is specified, the value is formatted into an appropriate label.
        this.ticks = [];
        // prop: numberTicks
        // Desired number of ticks.  Default is to compute automatically.
        this.numberTicks;
        // prop: tickInterval
        // number of units between ticks.  Mutually exclusive with numberTicks.
        this.tickInterval;
        // prop: renderer
        // A class of a rendering engine that handles tick generation, 
        // scaling input data to pixel grid units and drawing the axis element.
        this.renderer = $.jqplot.LinearAxisRenderer;
        // prop: rendererOptions
        // renderer specific options.  See <$.jqplot.LinearAxisRenderer> for options.
        this.rendererOptions = {};
        // prop: showTicks
        // Wether to show the ticks (both marks and labels) or not.
        // Will not override showMark and showLabel options if specified on the ticks themselves.
        this.showTicks = true;
        // prop: showTickMarks
        // Wether to show the tick marks (line crossing grid) or not.
        // Overridden by showTicks and showMark option of tick itself.
        this.showTickMarks = true;
        // prop: showMinorTicks
        // Wether or not to show minor ticks.  This is renderer dependent.
        // The default <$.jqplot.LinearAxisRenderer> does not have minor ticks.
        this.showMinorTicks = true;
        // prop: useSeriesColor
        // Use the color of the first series associated with this axis for the
        // tick marks and line bordering this axis.
        this.useSeriesColor = false;
        // prop: borderWidth
        // width of line stroked at the border of the axis.  Defaults
        // to the width of the grid boarder.
        this.borderWidth = null;
        // prop: borderColor
        // color of the border adjacent to the axis.  Defaults to grid border color.
        this.borderColor = null;
        // minimum and maximum values on the axis.
        this._dataBounds = {min:null, max:null};
        // pixel position from the top left of the min value and max value on the axis.
        this._offsets = {min:null, max:null};
        this._ticks=[];
        this._label = null;
        // prop: syncTicks
        // true to try and synchronize tick spacing across multiple axes so that ticks and
        // grid lines line up.  This has an impact on autoscaling algorithm, however.
        // In general, autoscaling an individual axis will work better if it does not
        // have to sync ticks.
        this.syncTicks = null;
        // prop: tickSpacing
        // Approximate pixel spacing between ticks on graph.  Used during autoscaling.
        // This number will be an upper bound, actual spacing will be less.
        this.tickSpacing = 75;
        // Properties to hold the original values for min, max, ticks, tickInterval and numberTicks
        // so they can be restored if altered by plugins.
        this._min = null;
        this._max = null;
        this._tickInterval = null;
        this._numberTicks = null;
        this.__ticks = null;
    }
    
    Axis.prototype = new $.jqplot.ElemContainer();
    Axis.prototype.constructor = Axis;
    
    Axis.prototype.init = function() {
        this.renderer = new this.renderer();
        // set the axis name
        this.tickOptions.axis = this.name;
        // if showMark or showLabel tick options not specified, use value of axis option.
        // showTicks overrides showTickMarks.
        if (this.tickOptions.showMark == null) {
            this.tickOptions.showMark = this.showTicks;
        }
        if (this.tickOptions.showMark == null) {
            this.tickOptions.showMark = this.showTickMarks;
        }
        if (this.tickOptions.showLabel == null) {
            this.tickOptions.showLabel = this.showTicks;
        }
        
        if (this.label == null || this.label == '') {
            this.showLabel = false;
        }
        else {
            this.labelOptions.label = this.label;
        }
        if (this.showLabel == false) {
            this.labelOptions.show = false;
        }
        // set the default padMax, padMin if not specified
        // special check, if no padding desired, padding
        // should be set to 1.0
        if (this.pad == 0) {
            this.pad = 1.0;
        }
        if (this.padMax == 0) {
            this.padMax = 1.0;
        }
        if (this.padMin == 0) {
            this.padMin = 1.0;
        }
        if (this.padMax == null) {
            this.padMax = (this.pad-1)/2 + 1;
        }
        if (this.padMin == null) {
            this.padMin = (this.pad-1)/2 + 1;
        }
        // now that padMin and padMax are correctly set, reset pad in case user has supplied 
        // padMin and/or padMax
        this.pad = this.padMax + this.padMin - 1;
        if (this.min != null || this.max != null) {
            this.autoscale = false;
        }
        // if not set, sync ticks for y axes but not x by default.
        if (this.syncTicks == null && this.name.indexOf('y') > -1) {
            this.syncTicks = true;
        }
        else if (this.syncTicks == null){
            this.syncTicks = false;
        }
        this.renderer.init.call(this, this.rendererOptions);
        
    };
    
    Axis.prototype.draw = function(ctx) {
        return this.renderer.draw.call(this, ctx);
        
    };
    
    Axis.prototype.set = function() {
        this.renderer.set.call(this);
    };
    
    Axis.prototype.pack = function(pos, offsets) {
        if (this.show) {
            this.renderer.pack.call(this, pos, offsets);
        }
        // these properties should all be available now.
        if (this._min == null) {
            this._min = this.min;
            this._max = this.max;
            this._tickInterval = this.tickInterval;
            this._numberTicks = this.numberTicks;
            this.__ticks = this._ticks;
        }
    };
    
    // reset the axis back to original values if it has been scaled, zoomed, etc.
    Axis.prototype.reset = function() {
        this.renderer.reset.call(this);
    };
    
    Axis.prototype.resetScale = function() {
        this.min = null;
        this.max = null;
        this.numberTicks = null;
        this.tickInterval = null;
    };

    /**
     * Class: Legend
     * Legend object.  Cannot be instantiated directly, but created
     * by the Plot oject.  Legend properties can be set or overriden by the 
     * options passed in from the user.
     */
    function Legend(options) {
        $.jqplot.ElemContainer.call(this);
        // Group: Properties
        
        // prop: show
        // Wether to display the legend on the graph.
        this.show = false;
        // prop: location
        // Placement of the legend.  one of the compass directions: nw, n, ne, e, se, s, sw, w
        this.location = 'ne';
        // prop: labels
        // Array of labels to use.  By default the renderer will look for labels on the series.
        // Labels specified in this array will override labels specified on the series.
        this.labels = [];
        // prop: showLabels
        // true to show the label text on the  legend.
        this.showLabels = true;
        // prop: showSwatch
        // true to show the color swatches on the legend.
        this.showSwatches = true;
        // prop: placement
        // "insideGrid" places legend inside the grid area of the plot.
        // "outsideGrid" places the legend outside the grid but inside the plot container, 
        // shrinking the grid to accomodate the legend.
        // "inside" synonym for "insideGrid", 
        // "outside" places the legend ouside the grid area, but does not shrink the grid which
        // can cause the legend to overflow the plot container.
        this.placement = "insideGrid";
        // prop: xoffset
        // DEPRECATED.  Set the margins on the legend using the marginTop, marginLeft, etc. 
        // properties or via CSS margin styling of the .jqplot-table-legend class.
        this.xoffset = 0;
        // prop: yoffset
        // DEPRECATED.  Set the margins on the legend using the marginTop, marginLeft, etc. 
        // properties or via CSS margin styling of the .jqplot-table-legend class.
        this.yoffset = 0;
        // prop: border
        // css spec for the border around the legend box.
        this.border;
        // prop: background
        // css spec for the background of the legend box.
        this.background;
        // prop: textColor
        // css color spec for the legend text.
        this.textColor;
        // prop: fontFamily
        // css font-family spec for the legend text.
        this.fontFamily; 
        // prop: fontSize
        // css font-size spec for the legend text.
        this.fontSize ;
        // prop: rowSpacing
        // css padding-top spec for the rows in the legend.
        this.rowSpacing = '0.5em';
        // renderer
        // A class that will create a DOM object for the legend,
        // see <$.jqplot.TableLegendRenderer>.
        this.renderer = $.jqplot.TableLegendRenderer;
        // prop: rendererOptions
        // renderer specific options passed to the renderer.
        this.rendererOptions = {};
        // prop: predraw
        // Wether to draw the legend before the series or not.
        // Used with series specific legend renderers for pie, donut, mekko charts, etc.
        this.preDraw = false;
        // prop: marginTop
        // CSS margin for the legend DOM element. This will set an element 
        // CSS style for the margin which will override any style sheet setting.
        // The default will be taken from the stylesheet.
        this.marginTop = null;
        // prop: marginRight
        // CSS margin for the legend DOM element. This will set an element 
        // CSS style for the margin which will override any style sheet setting.
        // The default will be taken from the stylesheet.
        this.marginRight = null;
        // prop: marginBottom
        // CSS margin for the legend DOM element. This will set an element 
        // CSS style for the margin which will override any style sheet setting.
        // The default will be taken from the stylesheet.
        this.marginBottom = null;
        // prop: marginLeft
        // CSS margin for the legend DOM element. This will set an element 
        // CSS style for the margin which will override any style sheet setting.
        // The default will be taken from the stylesheet.
        this.marginLeft = null;
        
        this.escapeHtml = false;
        this._series = [];
        
        $.extend(true, this, options);
    }
    
    Legend.prototype = new $.jqplot.ElemContainer();
    Legend.prototype.constructor = Legend;
    
    Legend.prototype.setOptions = function(options) {
        $.extend(true, this, options);
        
        // Try to emulate deprecated behaviour
        // if user has specified xoffset or yoffset, copy these to
        // the margin properties.
        
        if (this.placement ==  'inside') this.placement = 'insideGrid';
        
        if (this.xoffset >0) {
            if (this.placement == 'insideGrid') {
                switch (this.location) {
                    case 'nw':
                    case 'w':
                    case 'sw':
                        if (this.marginLeft == null) {
                            this.marginLeft = this.xoffset + 'px';
                        }
                        this.marginRight = '0px';
                        break;
                    case 'ne':
                    case 'e':
                    case 'se':
                    default:
                        if (this.marginRight == null) {
                            this.marginRight = this.xoffset + 'px';
                        }
                        this.marginLeft = '0px';
                        break;
                }
            }
            else if (this.placement == 'outside') {
                switch (this.location) {
                    case 'nw':
                    case 'w':
                    case 'sw':
                        if (this.marginRight == null) {
                            this.marginRight = this.xoffset + 'px';
                        }
                        this.marginLeft = '0px';
                        break;
                    case 'ne':
                    case 'e':
                    case 'se':
                    default:
                        if (this.marginLeft == null) {
                            this.marginLeft = this.xoffset + 'px';
                        }
                        this.marginRight = '0px';
                        break;
                }
            }
            this.xoffset = 0;
        }
        
        if (this.yoffset >0) {
            if (this.placement == 'outside') {
                switch (this.location) {
                    case 'sw':
                    case 's':
                    case 'se':
                        if (this.marginTop == null) {
                            this.marginTop = this.yoffset + 'px';
                        }
                        this.marginBottom = '0px';
                        break;
                    case 'ne':
                    case 'n':
                    case 'nw':
                    default:
                        if (this.marginBottom == null) {
                            this.marginBottom = this.yoffset + 'px';
                        }
                        this.marginTop = '0px';
                        break;
                }
            }
            else if (this.placement == 'insideGrid') {
                switch (this.location) {
                    case 'sw':
                    case 's':
                    case 'se':
                        if (this.marginBottom == null) {
                            this.marginBottom = this.yoffset + 'px';
                        }
                        this.marginTop = '0px';
                        break;
                    case 'ne':
                    case 'n':
                    case 'nw':
                    default:
                        if (this.marginTop == null) {
                            this.marginTop = this.yoffset + 'px';
                        }
                        this.marginBottom = '0px';
                        break;
                }
            }
            this.yoffset = 0;
        }
        
        // TO-DO:
        // Handle case where offsets are < 0.
        //
    };
    
    Legend.prototype.init = function() {
        this.renderer = new this.renderer();
        this.renderer.init.call(this, this.rendererOptions);
    };
    
    Legend.prototype.draw = function(offsets) {
        for (var i=0; i<$.jqplot.preDrawLegendHooks.length; i++){
            $.jqplot.preDrawLegendHooks[i].call(this, offsets);
        }
        return this.renderer.draw.call(this, offsets);
    };
    
    Legend.prototype.pack = function(offsets) {
        this.renderer.pack.call(this, offsets);
    };

    /**
     * Class: Title
     * Plot Title object.  Cannot be instantiated directly, but created
     * by the Plot oject.  Title properties can be set or overriden by the 
     * options passed in from the user.
     * 
     * Parameters:
     * text - text of the title.
     */
    function Title(text) {
        $.jqplot.ElemContainer.call(this);
        // Group: Properties
        
        // prop: text
        // text of the title;
        this.text = text;
        // prop: show
        // wether or not to show the title
        this.show = true;
        // prop: fontFamily
        // css font-family spec for the text.
        this.fontFamily;
        // prop: fontSize
        // css font-size spec for the text.
        this.fontSize ;
        // prop: textAlign
        // css text-align spec for the text.
        this.textAlign;
        // prop: textColor
        // css color spec for the text.
        this.textColor;
        // prop: renderer
        // A class for creating a DOM element for the title,
        // see <$.jqplot.DivTitleRenderer>.
        this.renderer = $.jqplot.DivTitleRenderer;
        // prop: rendererOptions
        // renderer specific options passed to the renderer.
        this.rendererOptions = {};   
    }
    
    Title.prototype = new $.jqplot.ElemContainer();
    Title.prototype.constructor = Title;
    
    Title.prototype.init = function() {
        this.renderer = new this.renderer();
        this.renderer.init.call(this, this.rendererOptions);
    };
    
    Title.prototype.draw = function(width) {
        return this.renderer.draw.call(this, width);
    };
    
    Title.prototype.pack = function() {
        this.renderer.pack.call(this);
    };


    /**
     * Class: Series
     * An individual data series object.  Cannot be instantiated directly, but created
     * by the Plot oject.  Series properties can be set or overriden by the 
     * options passed in from the user.
     */
    function Series() {
        $.jqplot.ElemContainer.call(this);
        // Group: Properties
        // Properties will be assigned from a series array at the top level of the
        // options.  If you had two series and wanted to change the color and line
        // width of the first and set the second to use the secondary y axis with
        // no shadow and supply custom labels for each:
        // > {
        // >    series:[
        // >        {color: '#ff4466', lineWidth: 5, label:'good line'},
        // >        {yaxis: 'y2axis', shadow: false, label:'bad line'}
        // >    ]
        // > }
        
        // prop: show
        // wether or not to draw the series.
        this.show = true;
        // prop: xaxis
        // which x axis to use with this series, either 'xaxis' or 'x2axis'.
        this.xaxis = 'xaxis';
        this._xaxis;
        // prop: yaxis
        // which y axis to use with this series, either 'yaxis' or 'y2axis'.
        this.yaxis = 'yaxis';
        this._yaxis;
        this.gridBorderWidth = 2.0;
        // prop: renderer
        // A class of a renderer which will draw the series, 
        // see <$.jqplot.LineRenderer>.
        this.renderer = $.jqplot.LineRenderer;
        // prop: rendererOptions
        // Options to pass on to the renderer.
        this.rendererOptions = {};
        this.data = [];
        this.gridData = [];
        // prop: label
        // Line label to use in the legend.
        this.label = '';
        // prop: showLabel
        // true to show label for this series in the legend.
        this.showLabel = true;
        // prop: color
        // css color spec for the series
        this.color;
        // prop: lineWidth
        // width of the line in pixels.  May have different meanings depending on renderer.
        this.lineWidth = 2.5;
        // prop: shadow
        // wether or not to draw a shadow on the line
        this.shadow = true;
        // prop: shadowAngle
        // Shadow angle in degrees
        this.shadowAngle = 45;
        // prop: shadowOffset
        // Shadow offset from line in pixels
        this.shadowOffset = 1.25;
        // prop: shadowDepth
        // Number of times shadow is stroked, each stroke offset shadowOffset from the last.
        this.shadowDepth = 3;
        // prop: shadowAlpha
        // Alpha channel transparency of shadow.  0 = transparent.
        this.shadowAlpha = '0.1';
        // prop: breakOnNull
        // Not implemented. wether line segments should be be broken at null value.
        // False will join point on either side of line.
        this.breakOnNull = false;
        // prop: markerRenderer
        // A class of a renderer which will draw marker (e.g. circle, square, ...) at the data points,
        // see <$.jqplot.MarkerRenderer>.
        this.markerRenderer = $.jqplot.MarkerRenderer;
        // prop: markerOptions
        // renderer specific options to pass to the markerRenderer,
        // see <$.jqplot.MarkerRenderer>.
        this.markerOptions = {};
        // prop: showLine
        // wether to actually draw the line or not.  Series will still be renderered, even if no line is drawn.
        this.showLine = true;
        // prop: showMarker
        // wether or not to show the markers at the data points.
        this.showMarker = true;
        // prop: index
        // 0 based index of this series in the plot series array.
        this.index;
        // prop: fill
        // true or false, wether to fill under lines or in bars.
        // May not be implemented in all renderers.
        this.fill = false;
        // prop: fillColor
        // CSS color spec to use for fill under line.  Defaults to line color.
        this.fillColor;
        // prop: fillAlpha
        // Alpha transparency to apply to the fill under the line.
        // Use this to adjust alpha separate from fill color.
        this.fillAlpha;
        // prop: fillAndStroke
        // If true will stroke the line (with color this.color) as well as fill under it.
        // Applies only when fill is true.
        this.fillAndStroke = false;
        // prop: disableStack
        // true to not stack this series with other series in the plot.
        // To render properly, non-stacked series must come after any stacked series
        // in the plot's data series array.  So, the plot's data series array would look like:
        // > [stackedSeries1, stackedSeries2, ..., nonStackedSeries1, nonStackedSeries2, ...]
        // disableStack will put a gap in the stacking order of series, and subsequent
        // stacked series will not fill down through the non-stacked series and will
        // most likely not stack properly on top of the non-stacked series.
        this.disableStack = false;
        // _stack is set by the Plot if the plot is a stacked chart.
        // will stack lines or bars on top of one another to build a "mountain" style chart.
        // May not be implemented in all renderers.
        this._stack = false;
        // prop: neighborThreshold
        // how close or far (in pixels) the cursor must be from a point marker to detect the point.
        this.neighborThreshold = 4;
        // prop: fillToZero
        // true will force bar and filled series to fill toward zero on the fill Axis.
        this.fillToZero = false;
        // prop: fillToValue
        // fill a filled series to this value on the fill axis.
        // Works in conjunction with fillToZero, so that must be true.
        this.fillToValue = 0;
        // prop: fillAxis
        // Either 'x' or 'y'.  Which axis to fill the line toward if fillToZero is true.
        // 'y' means fill up/down to 0 on the y axis for this series.
        this.fillAxis = 'y';
        // prop: useNegativeColors
        // true to color negative values differently in filled and bar charts.
        this.useNegativeColors = true;
        this._stackData = [];
        // _plotData accounts for stacking.  If plots not stacked, _plotData and data are same.  If
        // stacked, _plotData is accumulation of stacking data.
        this._plotData = [];
        // _plotValues hold the individual x and y values that will be plotted for this series.
        this._plotValues = {x:[], y:[]};
        // statistics about the intervals between data points.  Used for auto scaling.
        this._intervals = {x:{}, y:{}};
        // data from the previous series, for stacked charts.
        this._prevPlotData = [];
        this._prevGridData = [];
        this._stackAxis = 'y';
        this._primaryAxis = '_xaxis';
        // give each series a canvas to draw on.  This should allow for redrawing speedups.
        this.canvas = new $.jqplot.GenericCanvas();
        this.shadowCanvas = new $.jqplot.GenericCanvas();
        this.plugins = {};
        // sum of y values in this series.
        this._sumy = 0;
        this._sumx = 0;
    }
    
    Series.prototype = new $.jqplot.ElemContainer();
    Series.prototype.constructor = Series;
    
    Series.prototype.init = function(index, gridbw, plot) {
        // weed out any null values in the data.
        this.index = index;
        this.gridBorderWidth = gridbw;
        var d = this.data;
        var temp = [], i;
        for (i=0; i<d.length; i++) {
            if (! this.breakOnNull) {
                if (d[i] == null || d[i][0] == null || d[i][1] == null) {
                    continue;
                }
                else {
                    temp.push(d[i]);
                }
            }
            else {
                // TODO: figure out what to do with null values
                // probably involve keeping nulls in data array
                // and then updating renderers to break line
                // when it hits null value.
                // For now, just keep value.
                temp.push(d[i]);
            }
        }
        this.data = temp;
        if (!this.fillColor) {
            this.fillColor = this.color;
        }
        if (this.fillAlpha) {
            var comp = $.jqplot.normalize2rgb(this.fillColor);
            var comp = $.jqplot.getColorComponents(comp);
            this.fillColor = 'rgba('+comp[0]+','+comp[1]+','+comp[2]+','+this.fillAlpha+')';
        }
        this.renderer = new this.renderer();
        this.renderer.init.call(this, this.rendererOptions, plot);
        this.markerRenderer = new this.markerRenderer();
        if (!this.markerOptions.color) {
            this.markerOptions.color = this.color;
        }
        if (this.markerOptions.show == null) {
            this.markerOptions.show = this.showMarker;
        }
        this.showMarker = this.markerOptions.show;
        // the markerRenderer is called within it's own scaope, don't want to overwrite series options!!
        this.markerRenderer.init(this.markerOptions);
    };
    
    // data - optional data point array to draw using this series renderer
    // gridData - optional grid data point array to draw using this series renderer
    // stackData - array of cumulative data for stacked plots.
    Series.prototype.draw = function(sctx, opts, plot) {
        var options = (opts == undefined) ? {} : opts;
        sctx = (sctx == undefined) ? this.canvas._ctx : sctx;
        // hooks get called even if series not shown
        // we don't clear canvas here, it would wipe out all other series as well.
        for (var j=0; j<$.jqplot.preDrawSeriesHooks.length; j++) {
            $.jqplot.preDrawSeriesHooks[j].call(this, sctx, options);
        }
        if (this.show) {
            this.renderer.setGridData.call(this, plot);
            if (!options.preventJqPlotSeriesDrawTrigger) {
                $(sctx.canvas).trigger('jqplotSeriesDraw', [this.data, this.gridData]);
            }
            var data = [];
            if (options.data) {
                data = options.data;
            }
            else if (!this._stack) {
                data = this.data;
            }
            else {
                data = this._plotData;
            }
            var gridData = options.gridData || this.renderer.makeGridData.call(this, data, plot);
            this.renderer.draw.call(this, sctx, gridData, options, plot);
        }
        
        for (var j=0; j<$.jqplot.postDrawSeriesHooks.length; j++) {
            $.jqplot.postDrawSeriesHooks[j].call(this, sctx, options);
        }
    };
    
    Series.prototype.drawShadow = function(sctx, opts, plot) {
        var options = (opts == undefined) ? {} : opts;
        sctx = (sctx == undefined) ? this.shadowCanvas._ctx : sctx;
        // hooks get called even if series not shown
        // we don't clear canvas here, it would wipe out all other series as well.
        for (var j=0; j<$.jqplot.preDrawSeriesShadowHooks.length; j++) {
            $.jqplot.preDrawSeriesShadowHooks[j].call(this, sctx, options);
        }
        if (this.shadow) {
            this.renderer.setGridData.call(this, plot);

            var data = [];
            if (options.data) {
                data = options.data;
            }
            else if (!this._stack) {
                data = this.data;
            }
            else {
                data = this._plotData;
            }
            var gridData = options.gridData || this.renderer.makeGridData.call(this, data, plot);
        
            this.renderer.drawShadow.call(this, sctx, gridData, options);
        }
        
        for (var j=0; j<$.jqplot.postDrawSeriesShadowHooks.length; j++) {
            $.jqplot.postDrawSeriesShadowHooks[j].call(this, sctx, options);
        }
        
    };
    
    // toggles series display on plot, e.g. show/hide series
    Series.prototype.toggleDisplay = function(ev) {
        var s, speed;
        if (ev.data.series) {
            s = ev.data.series;
        }
        else {
            s = this;
        }
        if (ev.data.speed) {
            speed = ev.data.speed;
        }
        if (speed) {
            if (s.canvas._elem.is(':hidden')) {
                if (s.shadowCanvas._elem) {
                    s.shadowCanvas._elem.fadeIn(speed);
                }
                s.canvas._elem.fadeIn(speed);
                s.canvas._elem.nextAll('.jqplot-point-label.jqplot-series-'+s.index).fadeIn(speed);
            }
            else {
                if (s.shadowCanvas._elem) {
                    s.shadowCanvas._elem.fadeOut(speed);
                }
                s.canvas._elem.fadeOut(speed);
                s.canvas._elem.nextAll('.jqplot-point-label.jqplot-series-'+s.index).fadeOut(speed);
            }
        }
        else {
            if (s.canvas._elem.is(':hidden')) {
                if (s.shadowCanvas._elem) {
                    s.shadowCanvas._elem.show();
                }
                s.canvas._elem.show();
                s.canvas._elem.nextAll('.jqplot-point-label.jqplot-series-'+s.index).show();
            }
            else {
                if (s.shadowCanvas._elem) {
                    s.shadowCanvas._elem.hide();
                }
                s.canvas._elem.hide();
                s.canvas._elem.nextAll('.jqplot-point-label.jqplot-series-'+s.index).hide();
            }
        }
    };
    


    /**
     * Class: Grid
     * 
     * Object representing the grid on which the plot is drawn.  The grid in this
     * context is the area bounded by the axes, the area which will contain the series.
     * Note, the series are drawn on their own canvas.
     * The Grid object cannot be instantiated directly, but is created by the Plot oject.  
     * Grid properties can be set or overriden by the options passed in from the user.
     */
    function Grid() {
        $.jqplot.ElemContainer.call(this);
        // Group: Properties
        
        // prop: drawGridlines
        // wether to draw the gridlines on the plot.
        this.drawGridlines = true;
        // prop: gridLineColor
        // color of the grid lines.
        this.gridLineColor = '#cccccc';
        // prop: gridLineWidth
        // width of the grid lines.
        this.gridLineWidth = 1.0;
        // prop: background
        // css spec for the background color.
        this.background = '#fffdf6';
        // prop: borderColor
        // css spec for the color of the grid border.
        this.borderColor = '#999999';
        // prop: borderWidth
        // width of the border in pixels.
        this.borderWidth = 2.0;
        // prop: drawBorder
        // True to draw border around grid.
        this.drawBorder = true;
        // prop: shadow
        // wether to show a shadow behind the grid.
        this.shadow = true;
        // prop: shadowAngle
        // shadow angle in degrees
        this.shadowAngle = 45;
        // prop: shadowOffset
        // Offset of each shadow stroke from the border in pixels
        this.shadowOffset = 1.5;
        // prop: shadowWidth
        // width of the stoke for the shadow
        this.shadowWidth = 3;
        // prop: shadowDepth
        // Number of times shadow is stroked, each stroke offset shadowOffset from the last.
        this.shadowDepth = 3;
        // prop: shadowColor
        // an optional css color spec for the shadow in 'rgba(n, n, n, n)' form
        this.shadowColor = null;
        // prop: shadowAlpha
        // Alpha channel transparency of shadow.  0 = transparent.
        this.shadowAlpha = '0.07';
        this._left;
        this._top;
        this._right;
        this._bottom;
        this._width;
        this._height;
        this._axes = [];
        // prop: renderer
        // Instance of a renderer which will actually render the grid,
        // see <$.jqplot.CanvasGridRenderer>.
        this.renderer = $.jqplot.CanvasGridRenderer;
        // prop: rendererOptions
        // Options to pass on to the renderer,
        // see <$.jqplot.CanvasGridRenderer>.
        this.rendererOptions = {};
        this._offsets = {top:null, bottom:null, left:null, right:null};
    }
    
    Grid.prototype = new $.jqplot.ElemContainer();
    Grid.prototype.constructor = Grid;
    
    Grid.prototype.init = function() {
        this.renderer = new this.renderer();
        this.renderer.init.call(this, this.rendererOptions);
    };
    
    Grid.prototype.createElement = function(offsets) {
        this._offsets = offsets;
        return this.renderer.createElement.call(this);
    };
    
    Grid.prototype.draw = function() {
        this.renderer.draw.call(this);
    };
    
    $.jqplot.GenericCanvas = function() {
        $.jqplot.ElemContainer.call(this);
        this._ctx;  
    };
    
    $.jqplot.GenericCanvas.prototype = new $.jqplot.ElemContainer();
    $.jqplot.GenericCanvas.prototype.constructor = $.jqplot.GenericCanvas;
    
    $.jqplot.GenericCanvas.prototype.createElement = function(offsets, clss, plotDimensions) {
        this._offsets = offsets;
        var klass = 'jqplot';
        if (clss != undefined) {
            klass = clss;
        }
        var elem;
        // if this canvas already has a dom element, don't make a new one.
        if (this._elem) {
            elem = this._elem.get(0);
        }
        else {
            elem = document.createElement('canvas');
        }
        // if new plotDimensions supplied, use them.
        if (plotDimensions != undefined) {
            this._plotDimensions = plotDimensions;
        }
        
        elem.width = this._plotDimensions.width - this._offsets.left - this._offsets.right;
        elem.height = this._plotDimensions.height - this._offsets.top - this._offsets.bottom;
        this._elem = $(elem);
        this._elem.css({ position: 'absolute', left: this._offsets.left, top: this._offsets.top });
        
        this._elem.addClass(klass);
        if ($.jqplot.use_excanvas) {
            window.G_vmlCanvasManager.init_(document);
            elem = window.G_vmlCanvasManager.initElement(elem);
        }
        return this._elem;
    };
    
    $.jqplot.GenericCanvas.prototype.setContext = function() {
        this._ctx = this._elem.get(0).getContext("2d");
        return this._ctx;
    };
    
    $.jqplot.HooksManager = function () {
        this.hooks =[];
    };
    
    $.jqplot.HooksManager.prototype.addOnce = function(fn) {
        var havehook = false, i;
        for (i=0; i<this.hooks.length; i++) {
            if (this.hooks[i][0] == fn) {
                havehook = true;
            }
        }
        if (!havehook) {
            this.hooks.push(fn);
        }
    };
    
    $.jqplot.HooksManager.prototype.add = function(fn) {
        this.hooks.push(fn);
    };
    
    $.jqplot.EventListenerManager = function () {
        this.hooks =[];
    };
    
    $.jqplot.EventListenerManager.prototype.addOnce = function(ev, fn) {
        var havehook = false, h, i;
        for (i=0; i<this.hooks.length; i++) {
            h = this.hooks[i];
            if (h[0] == ev && h[1] == fn) {
                havehook = true;
            }
        }
        if (!havehook) {
            this.hooks.push([ev, fn]);
        }
    };
    
    $.jqplot.EventListenerManager.prototype.add = function(ev, fn) {
        this.hooks.push([ev, fn]);
    };

    /**
     * Class: jqPlot
     * Plot object returned by call to $.jqplot.  Handles parsing user options,
     * creating sub objects (Axes, legend, title, series) and rendering the plot.
     */
    function jqPlot() {
        // Group: Properties
        // These properties are specified at the top of the options object
        // like so:
        // > {
        // >     axesDefaults:{min:0},
        // >     series:[{color:'#6633dd'}],
        // >     title: 'A Plot'
        // > }
        //
        // prop: data
        // user's data.  Data should *NOT* be specified in the options object,
        // but be passed in as the second argument to the $.jqplot() function.
        // The data property is described here soley for reference. 
        // The data should be in the form of an array of 2D or 1D arrays like
        // > [ [[x1, y1], [x2, y2],...], [y1, y2, ...] ].
        this.data = [];
        // prop dataRenderer
        // A callable which can be used to preprocess data passed into the plot.
        // Will be called with 2 arguments, the plot data and a reference to the plot.
        this.dataRenderer;
        // prop dataRendererOptions
        // Options that will be passed to the dataRenderer.
        // Can be of any type.
        this.dataRendererOptions;
        // The id of the dom element to render the plot into
        this.targetId = null;
        // the jquery object for the dom target.
        this.target = null; 
        this.defaults = {
            // prop: axesDefaults
            // default options that will be applied to all axes.
            // see <Axis> for axes options.
            axesDefaults: {},
            axes: {xaxis:{}, yaxis:{}, x2axis:{}, y2axis:{}, y3axis:{}, y4axis:{}, y5axis:{}, y6axis:{}, y7axis:{}, y8axis:{}, y9axis:{}},
            // prop: seriesDefaults
            // default options that will be applied to all series.
            // see <Series> for series options.
            seriesDefaults: {},
            gridPadding: {top:10, right:10, bottom:23, left:10},
            series:[]
        };
        // prop: series
        // Array of series object options.
        // see <Series> for series specific options.
        this.series = [];
        // prop: axes
        // up to 4 axes are supported, each with it's own options, 
        // See <Axis> for axis specific options.
        this.axes = {xaxis: new Axis('xaxis'), yaxis: new Axis('yaxis'), x2axis: new Axis('x2axis'), y2axis: new Axis('y2axis'), y3axis: new Axis('y3axis'), y4axis: new Axis('y4axis'), y5axis: new Axis('y5axis'), y6axis: new Axis('y6axis'), y7axis: new Axis('y7axis'), y8axis: new Axis('y8axis'), y9axis: new Axis('y9axis')};
        // prop: grid
        // See <Grid> for grid specific options.
        this.grid = new Grid();
        // prop: legend
        // see <$.jqplot.TableLegendRenderer>
        this.legend = new Legend();
        this.baseCanvas = new $.jqplot.GenericCanvas();
        // array of series indicies. Keep track of order
        // which series canvases are displayed, lowest
        // to highest, back to front.
        this.seriesStack = [];
        this.previousSeriesStack = [];
        this.eventCanvas = new $.jqplot.GenericCanvas();
        this._width = null;
        this._height = null; 
        this._plotDimensions = {height:null, width:null};
        this._gridPadding = {top:10, right:10, bottom:10, left:10};
        // a shortcut for axis syncTicks options.  Not implemented yet.
        this.syncXTicks = true;
        // a shortcut for axis syncTicks options.  Not implemented yet.
        this.syncYTicks = true;
        // prop: seriesColors
        // Ann array of CSS color specifications that will be applied, in order,
        // to the series in the plot.  Colors will wrap around so, if their
        // are more series than colors, colors will be reused starting at the
        // beginning.  For pie charts, this specifies the colors of the slices.
        this.seriesColors = [ "#4bb2c5", "#EAA228", "#c5b47f", "#579575", "#839557", "#958c12", "#953579", "#4b5de4", "#d8b83f", "#ff5800", "#0085cc", "#c747a3", "#cddf54", "#FBD178", "#26B4E3", "#bd70c7"];
        this.negativeSeriesColors = [ "#498991", "#C08840", "#9F9274", "#546D61", "#646C4A", "#6F6621", "#6E3F5F", "#4F64B0", "#A89050", "#C45923", "#187399", "#945381", "#959E5C", "#C7AF7B", "#478396", "#907294"];
        // prop: sortData
        // false to not sort the data passed in by the user.
        // Many bar, stakced and other graphs as well as many plugins depend on
        // having sorted data.
        this.sortData = true;
        var seriesColorsIndex = 0;
        // prop textColor
        // css spec for the css color attribute.  Default for the entire plot.
        this.textColor;
        // prop; fontFamily
        // css spec for the font-family attribute.  Default for the entire plot.
        this.fontFamily;
        // prop: fontSize
        // css spec for the font-size attribute.  Default for the entire plot.
        this.fontSize;
        // prop: title
        // Title object.  See <Title> for specific options.  As a shortcut, you
        // can specify the title option as just a string like: title: 'My Plot'
        // and this will create a new title object with the specified text.
        this.title = new Title();
        // container to hold all of the merged options.  Convienence for plugins.
        this.options = {};
        // prop: stackSeries
        // true or false, creates a stack or "mountain" plot.
        // Not all series renderers may implement this option.
        this.stackSeries = false;
        // prop: defaultAxisStart
        // 1-D data series are internally converted into 2-D [x,y] data point arrays
        // by jqPlot.  This is the default starting value for the missing x or y value.
        // The added data will be a monotonically increasing series (e.g. [1, 2, 3, ...])
        // starting at this value.
        this.defaultAxisStart = 1;
        // array to hold the cumulative stacked series data.
        // used to ajust the individual series data, which won't have access to other
        // series data.
        this._stackData = [];
        // array that holds the data to be plotted. This will be the series data
        // merged with the the appropriate data from _stackData according to the stackAxis.
        this._plotData = [];
        // Namespece to hold plugins.  Generally non-renderer plugins add themselves to here.
        this.plugins = {};
        // Count how many times the draw method has been called while the plot is visible.
        // Mostly used to test if plot has never been dran (=0), has been successfully drawn
        // into a visible container once (=1) or draw more than once into a visible container.
        // Can use this in tests to see if plot has been visibly drawn at least one time.
        // After plot has been visibly drawn once, it generally doesn't need redrawn if its
        // container is hidden and shown.
        this._drawCount = 0;
        // this.doCustomEventBinding = true;
        // prop: drawIfHidden
        // True to execute the draw method even if the plot target is hidden.
        // Generally, this should be false.  Most plot elements will not be sized/
        // positioned correclty if renderered into a hidden container.  To render into
        // a hidden container, call the replot method when the container is shown.
        this.drawIfHidden = false;
        // true to intercept right click events and fire a 'jqplotRightClick' event.
        // this will also block the context menu.
        this.captureRightClick = false;
        this.themeEngine = new $.jqplot.ThemeEngine();
        // sum of y values for all series in plot.
        // used in mekko chart.
        this._sumy = 0;
        this._sumx = 0;
        this.preInitHooks = new $.jqplot.HooksManager();
        this.postInitHooks = new $.jqplot.HooksManager();
        this.preParseOptionsHooks = new $.jqplot.HooksManager();
        this.postParseOptionsHooks = new $.jqplot.HooksManager();
        this.preDrawHooks = new $.jqplot.HooksManager();
        this.postDrawHooks = new $.jqplot.HooksManager();
        this.preDrawSeriesHooks = new $.jqplot.HooksManager();
        this.postDrawSeriesHooks = new $.jqplot.HooksManager();
        this.preDrawLegendHooks = new $.jqplot.HooksManager();
        this.addLegendRowHooks = new $.jqplot.HooksManager();
        this.preSeriesInitHooks = new $.jqplot.HooksManager();
        this.postSeriesInitHooks = new $.jqplot.HooksManager();
        this.preParseSeriesOptionsHooks = new $.jqplot.HooksManager();
        this.postParseSeriesOptionsHooks = new $.jqplot.HooksManager();
        this.eventListenerHooks = new $.jqplot.EventListenerManager();
        this.preDrawSeriesShadowHooks = new $.jqplot.HooksManager();
        this.postDrawSeriesShadowHooks = new $.jqplot.HooksManager();
        
        this.colorGenerator = $.jqplot.ColorGenerator;
        
        // Group: methods
        //
        // method: init
        // sets the plot target, checks data and applies user
        // options to plot.
        this.init = function(target, data, options) {
            for (var i=0; i<$.jqplot.preInitHooks.length; i++) {
                $.jqplot.preInitHooks[i].call(this, target, data, options);
            }

            for (var i=0; i<this.preInitHooks.hooks.length; i++) {
                this.preInitHooks.hooks[i].call(this, target, data, options);
            }
            
            this.targetId = '#'+target;
            this.target = $('#'+target);
            // remove any error class that may be stuck on target.
            this.target.removeClass('jqplot-error');
            if (!this.target.get(0)) {
                throw "No plot target specified";
            }
            
            // make sure the target is positioned by some means and set css
            if (this.target.css('position') == 'static') {
                this.target.css('position', 'relative');
            }
            if (!this.target.hasClass('jqplot-target')) {
                this.target.addClass('jqplot-target');
            }
            
            // if no height or width specified, use a default.
            if (!this.target.height()) {
                var h;
                if (options && options.height) {
                    h = parseInt(options.height, 10);
                }
                else if (this.target.attr('data-height')) {
                    h = parseInt(this.target.attr('data-height'), 10);
                }
                else {
                    h = parseInt($.jqplot.config.defaultHeight, 10);
                }
                this._height = h;
                this.target.css('height', h+'px');
            }
            else {
                this._height = this.target.height();
            }
            if (!this.target.width()) {
                var w;
                if (options && options.width) {
                    w = parseInt(options.width, 10);
                }
                else if (this.target.attr('data-width')) {
                    w = parseInt(this.target.attr('data-width'), 10);
                }
                else {
                    w = parseInt($.jqplot.config.defaultWidth, 10);
                }
                this._width = w;
                this.target.css('width', w+'px');
            }
            else {
                this._width = this.target.width();
            }
            
            this._plotDimensions.height = this._height;
            this._plotDimensions.width = this._width;
            this.grid._plotDimensions = this._plotDimensions;
            this.title._plotDimensions = this._plotDimensions;
            this.baseCanvas._plotDimensions = this._plotDimensions;
            this.eventCanvas._plotDimensions = this._plotDimensions;
            this.legend._plotDimensions = this._plotDimensions;
            if (this._height <=0 || this._width <=0 || !this._height || !this._width) {
                throw "Canvas dimension not set";
            }
            
            if (options.dataRenderer && typeof(options.dataRenderer) == "function") {
                if (options.dataRendererOptions) {
                    this.dataRendererOptions = options.dataRendererOptions;
                }
                this.dataRenderer = options.dataRenderer;
                data = this.dataRenderer(data, this, this.dataRendererOptions);
            }
            
            if (data == null) {
                throw{
                    name: "DataError",
                    message: "No data to plot."
                };
            }
            
            if (data.constructor != Array || data.length == 0 || data[0].constructor != Array || data[0].length == 0) {
                throw{
                    name: "DataError",
                    message: "No data to plot."
                };
            }
            
            this.data = data;
            
            this.parseOptions(options);
            
            if (this.textColor) {
                this.target.css('color', this.textColor);
            }
            if (this.fontFamily) {
                this.target.css('font-family', this.fontFamily);
            }
            if (this.fontSize) {
                this.target.css('font-size', this.fontSize);
            }
            
            this.title.init();
            this.legend.init();
            this._sumy = 0;
            this._sumx = 0;
            for (var i=0; i<this.series.length; i++) {
                // set default stacking order for series canvases
                this.seriesStack.push(i);
                this.previousSeriesStack.push(i);
                this.series[i].shadowCanvas._plotDimensions = this._plotDimensions;
                this.series[i].canvas._plotDimensions = this._plotDimensions;
                for (var j=0; j<$.jqplot.preSeriesInitHooks.length; j++) {
                    $.jqplot.preSeriesInitHooks[j].call(this.series[i], target, data, this.options.seriesDefaults, this.options.series[i], this);
                }
                for (var j=0; j<this.preSeriesInitHooks.hooks.length; j++) {
                    this.preSeriesInitHooks.hooks[j].call(this.series[i], target, data, this.options.seriesDefaults, this.options.series[i], this);
                }
                this.populatePlotData(this.series[i], i);
                this.series[i]._plotDimensions = this._plotDimensions;
                this.series[i].init(i, this.grid.borderWidth, this);
                for (var j=0; j<$.jqplot.postSeriesInitHooks.length; j++) {
                    $.jqplot.postSeriesInitHooks[j].call(this.series[i], target, data, this.options.seriesDefaults, this.options.series[i], this);
                }
                for (var j=0; j<this.postSeriesInitHooks.hooks.length; j++) {
                    this.postSeriesInitHooks.hooks[j].call(this.series[i], target, data, this.options.seriesDefaults, this.options.series[i], this);
                }
                this._sumy += this.series[i]._sumy;
                this._sumx += this.series[i]._sumx;
            }

            for (var name in this.axes) {
                this.axes[name]._plotDimensions = this._plotDimensions;
                this.axes[name].init();
            }
            
            if (this.sortData) {
                sortData(this.series);
            }
            this.grid.init();
            this.grid._axes = this.axes;
            
            this.legend._series = this.series;

            for (var i=0; i<$.jqplot.postInitHooks.length; i++) {
                $.jqplot.postInitHooks[i].call(this, target, data, options);
            }

            for (var i=0; i<this.postInitHooks.hooks.length; i++) {
                this.postInitHooks.hooks[i].call(this, target, data, options);
            }
        };  
        
        // method: resetAxesScale
        // Reset the specified axes min, max, numberTicks and tickInterval properties to null
        // or reset these properties on all axes if no list of axes is provided.
        //
        // Parameters:
        // axes - Boolean to reset or not reset all axes or an array or object of axis names to reset.
        this.resetAxesScale = function(axes) {
            var ax = (axes != undefined) ? axes : this.axes;
            if (ax === true) {
                ax = this.axes;
            }
            if (ax.constructor === Array) {
                for (var i = 0; i < ax.length; i++) {
                    this.axes[ax[i]].resetScale();
                }
            }
            else if (ax.constructor === Object) {
                for (var name in ax) {
                    this.axes[name].resetScale();
                }
            }
        };
        // method: reInitialize
        // reinitialize plot for replotting.
        // not called directly.
        this.reInitialize = function () {
            // Plot should be visible and have a height and width.
            // If plot doesn't have height and width for some
            // reason, set it by other means.  Plot must not have
            // a display:none attribute, however.
            if (!this.target.height()) {
                var h;
                if (options && options.height) {
                    h = parseInt(options.height, 10);
                }
                else if (this.target.attr('data-height')) {
                    h = parseInt(this.target.attr('data-height'), 10);
                }
                else {
                    h = parseInt($.jqplot.config.defaultHeight, 10);
                }
                this._height = h;
                this.target.css('height', h+'px');
            }
            else {
                this._height = this.target.height();
            }
            if (!this.target.width()) {
                var w;
                if (options && options.width) {
                    w = parseInt(options.width, 10);
                }
                else if (this.target.attr('data-width')) {
                    w = parseInt(this.target.attr('data-width'), 10);
                }
                else {
                    w = parseInt($.jqplot.config.defaultWidth, 10);
                }
                this._width = w;
                this.target.css('width', w+'px');
            }
            else {
                this._width = this.target.width();
            }
            
            if (this._height <=0 || this._width <=0 || !this._height || !this._width) {
                throw "Target dimension not set";
            }
            
            this._plotDimensions.height = this._height;
            this._plotDimensions.width = this._width;
            this.grid._plotDimensions = this._plotDimensions;
            this.title._plotDimensions = this._plotDimensions;
            this.baseCanvas._plotDimensions = this._plotDimensions;
            this.eventCanvas._plotDimensions = this._plotDimensions;
            this.legend._plotDimensions = this._plotDimensions;
            
            for (var n in this.axes) {
                this.axes[n]._plotWidth = this._width;
                this.axes[n]._plotHeight = this._height;
            }
            
            this.title._plotWidth = this._width;
            
            if (this.textColor) {
                this.target.css('color', this.textColor);
            }
            if (this.fontFamily) {
                this.target.css('font-family', this.fontFamily);
            }
            if (this.fontSize) {
                this.target.css('font-size', this.fontSize);
            }
            
            this._sumy = 0;
            this._sumx = 0;
            for (var i=0; i<this.series.length; i++) {
                this.populatePlotData(this.series[i], i);
                this.series[i]._plotDimensions = this._plotDimensions;
                this.series[i].canvas._plotDimensions = this._plotDimensions;
                //this.series[i].init(i, this.grid.borderWidth);
                this._sumy += this.series[i]._sumy;
                this._sumx += this.series[i]._sumx;
            }
            
            for (var name in this.axes) {
                this.axes[name]._plotDimensions = this._plotDimensions;
                this.axes[name]._ticks = [];
                this.axes[name].renderer.init.call(this.axes[name], {});
            }
            
            if (this.sortData) {
                sortData(this.series);
            }
            
            this.grid._axes = this.axes;
            
            this.legend._series = this.series;
        };
        
        // sort the series data in increasing order.
        function sortData(series) {
            var d, sd, pd, ppd, ret;
            for (var i=0; i<series.length; i++) {
                // d = series[i].data;
                // sd = series[i]._stackData;
                // pd = series[i]._plotData;
                // ppd = series[i]._prevPlotData;
                var check;
                var bat = [series[i].data, series[i]._stackData, series[i]._plotData, series[i]._prevPlotData];
                for (var n=0; n<4; n++) {
                    check = true;
                    d = bat[n];
                    if (series[i]._stackAxis == 'x') {
                        for (var j = 0; j < d.length; j++) {
                            if (typeof(d[j][1]) != "number") {
                                check = false;
                                break;
                            }
                        }
                        if (check) {
                            d.sort(function(a,b) { return a[1] - b[1]; });
                            // sd.sort(function(a,b) { return a[1] - b[1]; });
                            // pd.sort(function(a,b) { return a[1] - b[1]; });
                            // ppd.sort(function(a,b) { return a[1] - b[1]; });
                        }
                    }
                    else {
                        for (var j = 0; j < d.length; j++) {
                            if (typeof(d[j][0]) != "number") {
                                check = false;
                                break;
                            }
                        }
                        if (check) {
                            d.sort(function(a,b) { return a[0] - b[0]; });
                            // sd.sort(function(a,b) { return a[0] - b[0]; });
                            // pd.sort(function(a,b) { return a[0] - b[0]; });
                            // ppd.sort(function(a,b) { return a[0] - b[0]; });
                        }
                    }
                }
               
            }
        }
        
        // populate the _stackData and _plotData arrays for the plot and the series.
        this.populatePlotData = function(series, index) {
            // if a stacked chart, compute the stacked data
            this._plotData = [];
            this._stackData = [];
            series._stackData = [];
            series._plotData = [];
            var plotValues = {x:[], y:[]};
            if (this.stackSeries && !series.disableStack) {
                series._stack = true;
                var sidx = series._stackAxis == 'x' ? 0 : 1;
                var idx = sidx ? 0 : 1;
                // push the current data into stackData
                //this._stackData.push(this.series[i].data);
                var temp = $.extend(true, [], series.data);
                // create the data that will be plotted for this series
                var plotdata = $.extend(true, [], series.data);
                // for first series, nothing to add to stackData.
                for (var j=0; j<index; j++) {
                    var cd = this.series[j].data;
                    for (var k=0; k<cd.length; k++) {
                        temp[k][0] += cd[k][0];
                        temp[k][1] += cd[k][1];
                        // only need to sum up the stack axis column of data
                        plotdata[k][sidx] += cd[k][sidx];
                    }
                }
                for (var i=0; i<plotdata.length; i++) {
                    plotValues.x.push(plotdata[i][0]);
                    plotValues.y.push(plotdata[i][1]);
                }
                this._plotData.push(plotdata);
                this._stackData.push(temp);
                series._stackData = temp;
                series._plotData = plotdata;
                series._plotValues = plotValues;
            }
            else {
                for (var i=0; i<series.data.length; i++) {
                    plotValues.x.push(series.data[i][0]);
                    plotValues.y.push(series.data[i][1]);
                }
                this._stackData.push(series.data);
                this.series[index]._stackData = series.data;
                this._plotData.push(series.data);
                series._plotData = series.data;
                series._plotValues = plotValues;
            }
            if (index>0) {
                series._prevPlotData = this.series[index-1]._plotData;
            }
            series._sumy = 0;
            series._sumx = 0;
            for (i=series.data.length-1; i>-1; i--) {
                series._sumy += series.data[i][1];
                series._sumx += series.data[i][0];
            }
        };
        
        // function to safely return colors from the color array and wrap around at the end.
        this.getNextSeriesColor = (function(t) {
            var idx = 0;
            var sc = t.seriesColors;
            
            return function () { 
                if (idx < sc.length) {
                    return sc[idx++];
                }
                else {
                    idx = 0;
                    return sc[idx++];
                }
            };
        })(this);
    
        this.parseOptions = function(options){
            for (var i=0; i<this.preParseOptionsHooks.hooks.length; i++) {
                this.preParseOptionsHooks.hooks[i].call(this, options);
            }
            for (var i=0; i<$.jqplot.preParseOptionsHooks.length; i++) {
                $.jqplot.preParseOptionsHooks[i].call(this, options);
            }
            this.options = $.extend(true, {}, this.defaults, options);
            this.stackSeries = this.options.stackSeries;
            if (this.options.seriesColors) {
                this.seriesColors = this.options.seriesColors;
            }
            if (this.options.negativeSeriesColors) {
                this.negativeSeriesColors = this.options.negativeSeriesColors;
            }
            if (this.options.captureRightClick) {
                this.captureRightClick = this.options.captureRightClick;
            }
            this.defaultAxisStart = (options && options.defaultAxisStart != null) ? options.defaultAxisStart : this.defaultAxisStart;
            var cg = new this.colorGenerator(this.seriesColors);
            // this._gridPadding = this.options.gridPadding;
            $.extend(true, this._gridPadding, this.options.gridPadding);
            this.sortData = (this.options.sortData != null) ? this.options.sortData : this.sortData;
            for (var n in this.axes) {
                var axis = this.axes[n];
                $.extend(true, axis, this.options.axesDefaults, this.options.axes[n]);
                axis._plotWidth = this._width;
                axis._plotHeight = this._height;
            }
            if (this.data.length == 0) {
                this.data = [];
                for (var i=0; i<this.options.series.length; i++) {
                    this.data.push(this.options.series.data);
                }    
            }
                
            var normalizeData = function(data, dir, start) {
                // return data as an array of point arrays,
                // in form [[x1,y1...], [x2,y2...], ...]
                var temp = [];
                var i;
                dir = dir || 'vertical';
                if (!(data[0] instanceof Array)) {
                    // we have a series of scalars.  One line with just y values.
                    // turn the scalar list of data into a data array of form:
                    // [[1, data[0]], [2, data[1]], ...]
                    for (i=0; i<data.length; i++) {
                        if (dir == 'vertical') {
                            temp.push([start + i, data[i]]);   
                        }
                        else {
                            temp.push([data[i], start+i]);
                        }
                    }
                }            
                else {
                    // we have a properly formatted data series, copy it.
                    $.extend(true, temp, data);
                }
                return temp;
            };

            for (var i=0; i<this.data.length; i++) { 
                var temp = new Series();
                for (var j=0; j<$.jqplot.preParseSeriesOptionsHooks.length; j++) {
                    $.jqplot.preParseSeriesOptionsHooks[j].call(temp, this.options.seriesDefaults, this.options.series[i]);
                }
                for (var j=0; j<this.preParseSeriesOptionsHooks.hooks.length; j++) {
                    this.preParseSeriesOptionsHooks.hooks[j].call(temp, this.options.seriesDefaults, this.options.series[i]);
                }
                $.extend(true, temp, {seriesColors:this.seriesColors, negativeSeriesColors:this.negativeSeriesColors}, this.options.seriesDefaults, this.options.series[i]);
                var dir = 'vertical';
                if (temp.renderer.constructor == $.jqplot.barRenderer && temp.rendererOptions && temp.rendererOptions.barDirection == 'horizontal') {
                    dir = 'horizontal';
                }
                temp.data = normalizeData(this.data[i], dir, this.defaultAxisStart);
                switch (temp.xaxis) {
                    case 'xaxis':
                        temp._xaxis = this.axes.xaxis;
                        break;
                    case 'x2axis':
                        temp._xaxis = this.axes.x2axis;
                        break;
                    default:
                        break;
                }
                temp._yaxis = this.axes[temp.yaxis];
                temp._xaxis._series.push(temp);
                temp._yaxis._series.push(temp);
                if (temp.show) {
                    temp._xaxis.show = true;
                    temp._yaxis.show = true;
                }

                // parse the renderer options and apply default colors if not provided
                if (!temp.color && temp.show != false) {
                    temp.color = cg.next();
                }
                if (!temp.label) {
                    temp.label = 'Series '+ (i+1).toString();
                }
                // temp.rendererOptions.show = temp.show;
                // $.extend(true, temp.renderer, {color:this.seriesColors[i]}, this.rendererOptions);
                this.series.push(temp);  
                for (var j=0; j<$.jqplot.postParseSeriesOptionsHooks.length; j++) {
                    $.jqplot.postParseSeriesOptionsHooks[j].call(this.series[i], this.options.seriesDefaults, this.options.series[i]);
                }
                for (var j=0; j<this.postParseSeriesOptionsHooks.hooks.length; j++) {
                    this.postParseSeriesOptionsHooks.hooks[j].call(this.series[i], this.options.seriesDefaults, this.options.series[i]);
                }
            }
            
            // copy the grid and title options into this object.
            $.extend(true, this.grid, this.options.grid);
            // if axis border properties aren't set, set default.
            for (var n in this.axes) {
                var axis = this.axes[n];
                if (axis.borderWidth == null) {
                    axis.borderWidth =this.grid.borderWidth;
                }
                if (axis.borderColor == null) {
                    if (n != 'xaxis' && n != 'x2axis' && axis.useSeriesColor === true && axis.show) {
                        axis.borderColor = axis._series[0].color;
                    }
                    else {
                        axis.borderColor = this.grid.borderColor;
                    }
                }
            }
            
            if (typeof this.options.title == 'string') {
                this.title.text = this.options.title;
            }
            else if (typeof this.options.title == 'object') {
                $.extend(true, this.title, this.options.title);
            }
            this.title._plotWidth = this._width;
            this.legend.setOptions(this.options.legend);
            
            for (var i=0; i<$.jqplot.postParseOptionsHooks.length; i++) {
                $.jqplot.postParseOptionsHooks[i].call(this, options);
            }
            for (var i=0; i<this.postParseOptionsHooks.hooks.length; i++) {
                this.postParseOptionsHooks.hooks[i].call(this, options);
            }
        };
        
        // method: replot
        // Does a reinitialization of the plot followed by
        // a redraw.  Method could be used to interactively
        // change plot characteristics and then replot.
        //
        // Parameters:
        // options - Options used for replotting.
        //
        // Properties:
        // clear - false to not clear (empty) the plot container before replotting (default: true).
        // resetAxes - true to reset all axes min, max, numberTicks and tickInterval setting so axes will rescale themselves.
        //             optionally pass in list of axes to reset (e.g. ['xaxis', 'y2axis']) (default: false).
        this.replot = function(options) {
            var opts = (options != undefined) ? options : {};
            var clear = (opts.clear != undefined) ? opts.clear : true;
            var resetAxes = (opts.resetAxes != undefined) ? opts.resetAxes : false;
            this.target.trigger('jqplotPreReplot');
            if (clear) {
                this.target.empty();
            }
            if (resetAxes) {
                this.resetAxesScale(resetAxes);
            }
            this.reInitialize();
            this.draw();
            this.target.trigger('jqplotPostReplot');
        };
        
        // method: redraw
        // Empties the plot target div and redraws the plot.
        // This enables plot data and properties to be changed
        // and then to comletely clear the plot and redraw.
        // redraw *will not* reinitialize any plot elements.
        // That is, axes will not be autoscaled and defaults
        // will not be reapplied to any plot elements.  redraw
        // is used primarily with zooming. 
        //
        // Parameters:
        // clear - false to not clear (empty) the plot container before redrawing (default: true).
        this.redraw = function(clear) {
            clear = (clear != null) ? clear : true;
            this.target.trigger('jqplotPreRedraw');
            if (clear) {
                this.target.empty();
            }
             for (var ax in this.axes) {
                this.axes[ax]._ticks = [];
            }
            for (var i=0; i<this.series.length; i++) {
                this.populatePlotData(this.series[i], i);
            }
            this._sumy = 0;
            this._sumx = 0;
            for (i=0; i<this.series.length; i++) {
                this._sumy += this.series[i]._sumy;
                this._sumx += this.series[i]._sumx;
            }
            this.draw();
            this.target.trigger('jqplotPostRedraw');
        };
        
        // method: draw
        // Draws all elements of the plot into the container.
        // Does not clear the container before drawing.
        this.draw = function(){
            if (this.drawIfHidden || this.target.is(':visible')) {
                this.target.trigger('jqplotPreDraw');
                var i, j;
                for (i=0; i<$.jqplot.preDrawHooks.length; i++) {
                    $.jqplot.preDrawHooks[i].call(this);
                }
                for (i=0; i<this.preDrawHooks.hooks.length; i++) {
                    this.preDrawHooks.hooks[i].call(this);
                }
                // create an underlying canvas to be used for special features.
                this.target.append(this.baseCanvas.createElement({left:0, right:0, top:0, bottom:0}, 'jqplot-base-canvas'));
                this.baseCanvas.setContext();
                this.target.append(this.title.draw());
                this.title.pack({top:0, left:0});
                
                // make room  for the legend between the grid and the edge.
                var legendElem = this.legend.draw();
                
                var gridPadding = {top:0, left:0, bottom:0, right:0};
                
                if (this.legend.placement == "outsideGrid") {
                    // temporarily append the legend to get dimensions
                    this.target.append(legendElem);
                    switch (this.legend.location) {
                        case 'n':
                            gridPadding.top += this.legend.getHeight();
                            break;
                        case 's':
                            gridPadding.bottom += this.legend.getHeight();
                            break;
                        case 'ne':
                        case 'e':
                        case 'se':
                            gridPadding.right += this.legend.getWidth();
                            break;
                        case 'nw':
                        case 'w':
                        case 'sw':
                            gridPadding.left += this.legend.getWidth();
                            break;
                        default:  // same as 'ne'
                            gridPadding.right += this.legend.getWidth();
                            break;
                    }
                    legendElem = legendElem.detach();
                }
                
                var ax = this.axes;
                for (var name in ax) {
                    this.target.append(ax[name].draw(this.baseCanvas._ctx));
                    ax[name].set();
                }
                if (ax.yaxis.show) {
                    gridPadding.left += ax.yaxis.getWidth();
                }
                var ra = ['y2axis', 'y3axis', 'y4axis', 'y5axis', 'y6axis', 'y7axis', 'y8axis', 'y9axis'];
                var rapad = [0, 0, 0, 0, 0, 0, 0, 0];
                var gpr = 0;
                var n;
                for (n=0; n<8; n++) {
                    if (ax[ra[n]].show) {
                        gpr += ax[ra[n]].getWidth();
                        rapad[n] = gpr;
                    }
                }
                gridPadding.right += gpr;
                if (ax.x2axis.show) {
                    gridPadding.top += ax.x2axis.getHeight();
                }
                if (this.title.show) {
                    gridPadding.top += this.title.getHeight();
                }
                if (ax.xaxis.show) {
                    gridPadding.bottom += ax.xaxis.getHeight();
                }
                
                // end of gridPadding adjustments.
                var arr = ['top', 'bottom', 'left', 'right'];
                for (var n in arr) {
                    if (gridPadding[arr[n]]) {
                        this._gridPadding[arr[n]] = gridPadding[arr[n]];
                    }
                }
                
                var legendPadding = (this.legend.placement == 'outsideGrid') ? {top:this.title.getHeight(), left: 0, right: 0, bottom: 0} : this._gridPadding;
            
                ax.xaxis.pack({position:'absolute', bottom:this._gridPadding.bottom - ax.xaxis.getHeight(), left:0, width:this._width}, {min:this._gridPadding.left, max:this._width - this._gridPadding.right});
                ax.yaxis.pack({position:'absolute', top:0, left:this._gridPadding.left - ax.yaxis.getWidth(), height:this._height}, {min:this._height - this._gridPadding.bottom, max: this._gridPadding.top});
                ax.x2axis.pack({position:'absolute', top:this._gridPadding.top - ax.x2axis.getHeight(), left:0, width:this._width}, {min:this._gridPadding.left, max:this._width - this._gridPadding.right});
                for (i=8; i>0; i--) {
                    ax[ra[i-1]].pack({position:'absolute', top:0, right:this._gridPadding.right - rapad[i-1]}, {min:this._height - this._gridPadding.bottom, max: this._gridPadding.top});
                }
                // ax.y2axis.pack({position:'absolute', top:0, right:0}, {min:this._height - this._gridPadding.bottom, max: this._gridPadding.top});
            
                this.target.append(this.grid.createElement(this._gridPadding));
                this.grid.draw();
                
                // put the shadow canvases behind the series canvases so shadows don't overlap on stacked bars.
                for (i=0; i<this.series.length; i++) {
                    // draw series in order of stacking.  This affects only
                    // order in which canvases are added to dom.
                    j = this.seriesStack[i];
                    this.target.append(this.series[j].shadowCanvas.createElement(this._gridPadding, 'jqplot-series-shadowCanvas'));
                    this.series[j].shadowCanvas.setContext();
                    this.series[j].shadowCanvas._elem.data('seriesIndex', j);
                }
                
                for (i=0; i<this.series.length; i++) {
                    // draw series in order of stacking.  This affects only
                    // order in which canvases are added to dom.
                    j = this.seriesStack[i];
                    this.target.append(this.series[j].canvas.createElement(this._gridPadding, 'jqplot-series-canvas'));
                    this.series[j].canvas.setContext();
                    this.series[j].canvas._elem.data('seriesIndex', j);
                }
                // Need to use filled canvas to capture events in IE.
                // Also, canvas seems to block selection of other elements in document on FF.
                this.target.append(this.eventCanvas.createElement(this._gridPadding, 'jqplot-event-canvas'));
                this.eventCanvas.setContext();
                this.eventCanvas._ctx.fillStyle = 'rgba(0,0,0,0)';
                this.eventCanvas._ctx.fillRect(0,0,this.eventCanvas._ctx.canvas.width, this.eventCanvas._ctx.canvas.height);
            
                // bind custom event handlers to regular events.
                this.bindCustomEvents();
            
                // draw legend before series if the series needs to know the legend dimensions.
                if (this.legend.preDraw) {  
                    this.eventCanvas._elem.before(legendElem);
                    this.legend.pack(legendPadding);
                    if (this.legend._elem) {
                        this.drawSeries({legendInfo:{location:this.legend.location, placement:this.legend.placement, width:this.legend.getWidth(), height:this.legend.getHeight(), xoffset:this.legend.xoffset, yoffset:this.legend.yoffset}});
                    }
                    else {
                        this.drawSeries();
                    }
                }
                else {  // draw series before legend
                    this.drawSeries();
                    $(this.series[this.series.length-1].canvas._elem).after(legendElem);
                    this.legend.pack(legendPadding);                
                }
            
                // register event listeners on the overlay canvas
                for (var i=0; i<$.jqplot.eventListenerHooks.length; i++) {
                    // in the handler, this will refer to the eventCanvas dom element.
                    // make sure there are references back into plot objects.
                    this.eventCanvas._elem.bind($.jqplot.eventListenerHooks[i][0], {plot:this}, $.jqplot.eventListenerHooks[i][1]);
                }
            
                // register event listeners on the overlay canvas
                for (var i=0; i<this.eventListenerHooks.hooks.length; i++) {
                    // in the handler, this will refer to the eventCanvas dom element.
                    // make sure there are references back into plot objects.
                    this.eventCanvas._elem.bind(this.eventListenerHooks.hooks[i][0], {plot:this}, this.eventListenerHooks.hooks[i][1]);
                }

                for (var i=0; i<$.jqplot.postDrawHooks.length; i++) {
                    $.jqplot.postDrawHooks[i].call(this);
                }

                for (var i=0; i<this.postDrawHooks.hooks.length; i++) {
                    this.postDrawHooks.hooks[i].call(this);
                }
            
                if (this.target.is(':visible')) {
                    this._drawCount += 1;
                }
            
                this.target.trigger('jqplotPostDraw', [this]);
            }
        };
        
        this.bindCustomEvents = function() {
            this.eventCanvas._elem.bind('click', {plot:this}, this.onClick);
            this.eventCanvas._elem.bind('dblclick', {plot:this}, this.onDblClick);
            this.eventCanvas._elem.bind('mousedown', {plot:this}, this.onMouseDown);
            this.eventCanvas._elem.bind('mousemove', {plot:this}, this.onMouseMove);
            this.eventCanvas._elem.bind('mouseenter', {plot:this}, this.onMouseEnter);
            this.eventCanvas._elem.bind('mouseleave', {plot:this}, this.onMouseLeave);
            if (this.captureRightClick) {
                this.eventCanvas._elem.bind('mouseup', {plot:this}, this.onRightClick);
                this.eventCanvas._elem.get(0).oncontextmenu = function() {
					return false;
				};
            }
            else {
                this.eventCanvas._elem.bind('mouseup', {plot:this}, this.onMouseUp);
            }
        };
        
        function getEventPosition(ev) {
            var plot = ev.data.plot;
            var go = plot.eventCanvas._elem.offset();
            var gridPos = {x:ev.pageX - go.left, y:ev.pageY - go.top};
            var dataPos = {xaxis:null, yaxis:null, x2axis:null, y2axis:null, y3axis:null, y4axis:null, y5axis:null, y6axis:null, y7axis:null, y8axis:null, y9axis:null};
            var an = ['xaxis', 'yaxis', 'x2axis', 'y2axis', 'y3axis', 'y4axis', 'y5axis', 'y6axis', 'y7axis', 'y8axis', 'y9axis'];
            var ax = plot.axes;
            var n, axis;
            for (n=11; n>0; n--) {
                axis = an[n-1];
                if (ax[axis].show) {
                    dataPos[axis] = ax[axis].series_p2u(gridPos[axis.charAt(0)]);
                }
            }

            return {offsets:go, gridPos:gridPos, dataPos:dataPos};
        }
        
        
        // function to check if event location is over a area area
        function checkIntersection(gridpos, plot) {
            var series = plot.series;
            var i, j, k, s, r, x, y, theta, sm, sa, minang, maxang;
            var d0, d, p, pp, points, bw;
            var threshold, t;
            for (k=plot.seriesStack.length-1; k>=0; k--) {
                i = plot.seriesStack[k];
                s = series[i];
                switch (s.renderer.constructor) {
                    case $.jqplot.BarRenderer:
                        x = gridpos.x;
                        y = gridpos.y;
                        for (j=s.gridData.length-1; j>=0; j--) {
                            points = s._barPoints[j];
                            if (x>points[0][0] && x<points[2][0] && y>points[2][1] && y<points[0][1]) {
                                return {seriesIndex:s.index, pointIndex:j, gridData:p, data:s.data[j], points:s._barPoints[j]};
                            }
                        }
                        break;
                    
                    case $.jqplot.DonutRenderer:
                        sa = s.startAngle/180*Math.PI;
                        x = gridpos.x - s._center[0];
                        y = gridpos.y - s._center[1];
                        r = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
                        if (x > 0 && -y >= 0) {
                            theta = 2*Math.PI - Math.atan(-y/x);
                        }
                        else if (x > 0 && -y < 0) {
                            theta = -Math.atan(-y/x);
                        }
                        else if (x < 0) {
                            theta = Math.PI - Math.atan(-y/x);
                        }
                        else if (x == 0 && -y > 0) {
                            theta = 3*Math.PI/2;
                        }
                        else if (x == 0 && -y < 0) {
                            theta = Math.PI/2;
                        }
                        else if (x == 0 && y == 0) {
                            theta = 0;
                        }
                        if (sa) {
                            theta -= sa;
                            if (theta < 0) {
                                theta += 2*Math.PI;
                            }
                            else if (theta > 2*Math.PI) {
                                theta -= 2*Math.PI;
                            }
                        }
            
                        sm = s.sliceMargin/180*Math.PI;
                        if (r < s._radius && r > s._innerRadius) {
                            for (j=0; j<s.gridData.length; j++) {
                                minang = (j>0) ? s.gridData[j-1][1]+sm : sm;
                                maxang = s.gridData[j][1];
                                if (theta > minang && theta < maxang) {
                                    return {seriesIndex:s.index, pointIndex:j, gridData:s.gridData[j], data:s.data[j]};
                                }
                            }
                        }
                        break;
                        
                    case $.jqplot.PieRenderer:
                        sa = s.startAngle/180*Math.PI;
                        x = gridpos.x - s._center[0];
                        y = gridpos.y - s._center[1];
                        r = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
                        if (x > 0 && -y >= 0) {
                            theta = 2*Math.PI - Math.atan(-y/x);
                        }
                        else if (x > 0 && -y < 0) {
                            theta = -Math.atan(-y/x);
                        }
                        else if (x < 0) {
                            theta = Math.PI - Math.atan(-y/x);
                        }
                        else if (x == 0 && -y > 0) {
                            theta = 3*Math.PI/2;
                        }
                        else if (x == 0 && -y < 0) {
                            theta = Math.PI/2;
                        }
                        else if (x == 0 && y == 0) {
                            theta = 0;
                        }
                        if (sa) {
                            theta -= sa;
                            if (theta < 0) {
                                theta += 2*Math.PI;
                            }
                            else if (theta > 2*Math.PI) {
                                theta -= 2*Math.PI;
                            }
                        }
            
                        sm = s.sliceMargin/180*Math.PI;
                        if (r < s._radius) {
                            for (j=0; j<s.gridData.length; j++) {
                                minang = (j>0) ? s.gridData[j-1][1]+sm : sm;
                                maxang = s.gridData[j][1];
                                if (theta > minang && theta < maxang) {
                                    return {seriesIndex:s.index, pointIndex:j, gridData:s.gridData[j], data:s.data[j]};
                                }
                            }
                        }
                        break;
                        
                    case $.jqplot.BubbleRenderer:
                        x = gridpos.x;
                        y = gridpos.y;
                        var ret = null;
                        
                        if (s.show) {
                            for (var j=0; j<s.gridData.length; j++) {
                                p = s.gridData[j];
                                d = Math.sqrt( (x-p[0]) * (x-p[0]) + (y-p[1]) * (y-p[1]) );
                                if (d <= p[2] && (d <= d0 || d0 == null)) {
                                   d0 = d;
                                   ret = {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                }
                            }
                            if (ret != null) return ret;
                        }
                        break;
                        
                    case $.jqplot.FunnelRenderer:
                        x = gridpos.x;
                        y = gridpos.y;
                        var v = s._vertices,
                            vfirst = v[0],
                            vlast = v[v.length-1],
                            lex,
                            rex;
    
                        // equations of right and left sides, returns x, y values given height of section (y value and 2 points)
    
                        function findedge (l, p1 , p2) {
                            var m = (p1[1] - p2[1])/(p1[0] - p2[0]);
                            var b = p1[1] - m*p1[0];
                            var y = l + p1[1];
        
                            return [(y - b)/m, y];
                        }
    
                        // check each section
                        lex = findedge(y, vfirst[0], vlast[3]);
                        rex = findedge(y, vfirst[1], vlast[2]);
                        for (j=0; j<v.length; j++) {
                            cv = v[j];
                            if (y >= cv[0][1] && y <= cv[3][1] && x >= lex[0] && x <= rex[0]) {
                                return {seriesIndex:s.index, pointIndex:j, gridData:null, data:s.data[j]};
                            }
                        }         
                        break;           
                    
                    case $.jqplot.LineRenderer:
                        x = gridpos.x;
                        y = gridpos.y;
                        r = s.renderer;
                        if (s.show) {
                            if (s.fill) {
                                // first check if it is in bounding box
                                var inside = false;
                                if (x>s._boundingBox[0][0] && x<s._boundingBox[1][0] && y>s._boundingBox[1][1] && y<s._boundingBox[0][1]) { 
                                    // now check the crossing number   
                                    
                                    var numPoints = s._areaPoints.length;
                                    var ii;
                                    var j = numPoints-1;

                                    for(var ii=0; ii < numPoints; ii++) { 
                                    	var vertex1 = [s._areaPoints[ii][0], s._areaPoints[ii][1]];
                                    	var vertex2 = [s._areaPoints[j][0], s._areaPoints[j][1]];

                                    	if (vertex1[1] < y && vertex2[1] >= y || vertex2[1] < y && vertex1[1] >= y)	 {
                                    		if (vertex1[0] + (y - vertex1[1]) / (vertex2[1] - vertex1[1]) * (vertex2[0] - vertex1[0]) < x) {
                                    			inside = !inside;
                                    		}
                                    	}

                                    	j = ii;
                                    }        
                                }
                                if (inside) {
                                    return {seriesIndex:i, pointIndex:null, gridData:s.gridData, data:s.data, points:s._areaPoints};
                                }
                                break;
                                
                            }
                            else {
                                t = s.markerRenderer.size/2+s.neighborThreshold;
                                threshold = (t > 0) ? t : 0;
                                for (var j=0; j<s.gridData.length; j++) {
                                    p = s.gridData[j];
                                    // neighbor looks different to OHLC chart.
                                    if (r.constructor == $.jqplot.OHLCRenderer) {
                                        if (r.candleStick) {
                                            var yp = s._yaxis.series_u2p;
                                            if (x >= p[0]-r._bodyWidth/2 && x <= p[0]+r._bodyWidth/2 && y >= yp(s.data[j][2]) && y <= yp(s.data[j][3])) {
                                                return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                            }
                                        }
                                        // if an open hi low close chart
                                        else if (!r.hlc){
                                            var yp = s._yaxis.series_u2p;
                                            if (x >= p[0]-r._tickLength && x <= p[0]+r._tickLength && y >= yp(s.data[j][2]) && y <= yp(s.data[j][3])) {
                                                return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                            }
                                        }
                                        // a hi low close chart
                                        else {
                                            var yp = s._yaxis.series_u2p;
                                            if (x >= p[0]-r._tickLength && x <= p[0]+r._tickLength && y >= yp(s.data[j][1]) && y <= yp(s.data[j][2])) {
                                                return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                            }
                                        }
                            
                                    }
                                    else if (p[0] != null && p[1] != null){
                                        d = Math.sqrt( (x-p[0]) * (x-p[0]) + (y-p[1]) * (y-p[1]) );
                                        if (d <= threshold && (d <= d0 || d0 == null)) {
                                           d0 = d;
                                           return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                        }
                                    }
                                } 
                            }
                        }
                        break;
                        
                    default:
                        x = gridpos.x;
                        y = gridpos.y;
                        r = s.renderer;
                        if (s.show) {
                            t = s.markerRenderer.size/2+s.neighborThreshold;
                            threshold = (t > 0) ? t : 0;
                            for (var j=0; j<s.gridData.length; j++) {
                                p = s.gridData[j];
                                // neighbor looks different to OHLC chart.
                                if (r.constructor == $.jqplot.OHLCRenderer) {
                                    if (r.candleStick) {
                                        var yp = s._yaxis.series_u2p;
                                        if (x >= p[0]-r._bodyWidth/2 && x <= p[0]+r._bodyWidth/2 && y >= yp(s.data[j][2]) && y <= yp(s.data[j][3])) {
                                            return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                        }
                                    }
                                    // if an open hi low close chart
                                    else if (!r.hlc){
                                        var yp = s._yaxis.series_u2p;
                                        if (x >= p[0]-r._tickLength && x <= p[0]+r._tickLength && y >= yp(s.data[j][2]) && y <= yp(s.data[j][3])) {
                                            return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                        }
                                    }
                                    // a hi low close chart
                                    else {
                                        var yp = s._yaxis.series_u2p;
                                        if (x >= p[0]-r._tickLength && x <= p[0]+r._tickLength && y >= yp(s.data[j][1]) && y <= yp(s.data[j][2])) {
                                            return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                        }
                                    }
                            
                                }
                                else {
                                    d = Math.sqrt( (x-p[0]) * (x-p[0]) + (y-p[1]) * (y-p[1]) );
                                    if (d <= threshold && (d <= d0 || d0 == null)) {
                                       d0 = d;
                                       return {seriesIndex: i, pointIndex:j, gridData:p, data:s.data[j]};
                                    }
                                }
                            } 
                        }
                        break;
                }
            }
            
            return null;
        }
        
        
        
        this.onClick = function(ev) {
            // Event passed in is normalized and will have data attribute.
            // Event passed out is unnormalized.
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var neighbor = checkIntersection(positions.gridPos, p);
            var evt = jQuery.Event('jqplotClick');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, neighbor, p]);
        };
        
        this.onDblClick = function(ev) {
            // Event passed in is normalized and will have data attribute.
            // Event passed out is unnormalized.
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var neighbor = checkIntersection(positions.gridPos, p);
            var evt = jQuery.Event('jqplotDblClick');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, neighbor, p]);
        };
        
        this.onMouseDown = function(ev) {
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var neighbor = checkIntersection(positions.gridPos, p);
            var evt = jQuery.Event('jqplotMouseDown');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, neighbor, p]);
        };
        
        this.onMouseUp = function(ev) {
            var positions = getEventPosition(ev);
            var evt = jQuery.Event('jqplotMouseUp');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, null, ev.data.plot]);
        };
        
        this.onRightClick = function(ev) {
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var neighbor = checkIntersection(positions.gridPos, p);
            if (p.captureRightClick) {
                if (ev.which == 3) {
                var evt = jQuery.Event('jqplotRightClick');
                evt.pageX = ev.pageX;
                evt.pageY = ev.pageY;
                    $(this).trigger(evt, [positions.gridPos, positions.dataPos, neighbor, p]);
                }
                else {
                var evt = jQuery.Event('jqplotMouseUp');
                evt.pageX = ev.pageX;
                evt.pageY = ev.pageY;
                    $(this).trigger(evt, [positions.gridPos, positions.dataPos, neighbor, p]);
                }
            }
        };
        
        this.onMouseMove = function(ev) {
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var neighbor = checkIntersection(positions.gridPos, p);
            var evt = jQuery.Event('jqplotMouseMove');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, neighbor, p]);
        };
        
        this.onMouseEnter = function(ev) {
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var evt = jQuery.Event('jqplotMouseEnter');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, null, p]);
        };
        
        this.onMouseLeave = function(ev) {
            var positions = getEventPosition(ev);
            var p = ev.data.plot;
            var evt = jQuery.Event('jqplotMouseLeave');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            $(this).trigger(evt, [positions.gridPos, positions.dataPos, null, p]);
        };
        
        // method: drawSeries
        // Redraws all or just one series on the plot.  No axis scaling
        // is performed and no other elements on the plot are redrawn.
        // options is an options object to pass on to the series renderers.
        // It can be an empty object {}.  idx is the series index
        // to redraw if only one series is to be redrawn.
        this.drawSeries = function(options, idx){
            var i, series, ctx;
            // if only one argument passed in and it is a number, use it ad idx.
            idx = (typeof(options) == "number" && idx == null) ? options : idx;
            options = (typeof(options) == "object") ? options : {};
            // draw specified series
            if (idx != undefined) {
                series = this.series[idx];
                ctx = series.shadowCanvas._ctx;
                ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
                series.drawShadow(ctx, options, this);
                ctx = series.canvas._ctx;
                ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
                series.draw(ctx, options, this);
                if (series.renderer.constructor == $.jqplot.BezierCurveRenderer) {
                    if (idx < this.series.length - 1) {
                        this.drawSeries(idx+1); 
                    }
                }
            }
            
            else {
                // if call series drawShadow method first, in case all series shadows
                // should be drawn before any series.  This will ensure, like for 
                // stacked bar plots, that shadows don't overlap series.
                for (i=0; i<this.series.length; i++) {
                    // first clear the canvas
                    series = this.series[i];
                    ctx = series.shadowCanvas._ctx;
                    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
                    series.drawShadow(ctx, options, this);
                    ctx = series.canvas._ctx;
                    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
                    series.draw(ctx, options, this);
                }
            }
        };
        
        // method: moveSeriesToFront
        // This method requires jQuery 1.4+
        // Moves the specified series canvas in front of all other series canvases.
        // This effectively "draws" the specified series on top of all other series,
        // although it is performed through DOM manipulation, no redrawing is performed.
        //
        // Parameters:
        // idx - 0 based index of the series to move.  This will be the index of the series
        // as it was first passed into the jqplot function.
        this.moveSeriesToFront = function (idx) { 
            idx = parseInt(idx, 10);
            var stackIndex = $.inArray(idx, this.seriesStack);
            // if already in front, return
            if (stackIndex == -1) {
                return;
            }
            if (stackIndex == this.seriesStack.length -1) {
                this.previousSeriesStack = this.seriesStack.slice(0);
                return;
            }
            var opidx = this.seriesStack[this.seriesStack.length -1];
            var serelem = this.series[idx].canvas._elem.detach();
            var shadelem = this.series[idx].shadowCanvas._elem.detach();
            this.series[opidx].shadowCanvas._elem.after(shadelem);
            this.series[opidx].canvas._elem.after(serelem);
            this.previousSeriesStack = this.seriesStack.slice(0);
            this.seriesStack.splice(stackIndex, 1);
            this.seriesStack.push(idx);
        };
        
        // method: moveSeriesToBack
        // This method requires jQuery 1.4+
        // Moves the specified series canvas behind all other series canvases.
        //
        // Parameters:
        // idx - 0 based index of the series to move.  This will be the index of the series
        // as it was first passed into the jqplot function.
        this.moveSeriesToBack = function (idx) {
            idx = parseInt(idx, 10);
            var stackIndex = $.inArray(idx, this.seriesStack);
            // if already in back, return
            if (stackIndex == 0 || stackIndex == -1) {
                return;
            }
            var opidx = this.seriesStack[0];
            var serelem = this.series[idx].canvas._elem.detach();
            var shadelem = this.series[idx].shadowCanvas._elem.detach();
            this.series[opidx].shadowCanvas._elem.before(shadelem);
            this.series[opidx].canvas._elem.before(serelem);
            this.previousSeriesStack = this.seriesStack.slice(0);
            this.seriesStack.splice(stackIndex, 1);
            this.seriesStack.unshift(idx);
        };
        
        // method: restorePreviousSeriesOrder
        // This method requires jQuery 1.4+
        // Restore the series canvas order to its previous state.
        // Useful to put a series back where it belongs after moving
        // it to the front.
        this.restorePreviousSeriesOrder = function () {
            var i, j, serelem, shadelem, temp;
            // if no change, return.
            if (this.seriesStack == this.previousSeriesStack) {
                return;
            }
            for (i=1; i<this.previousSeriesStack.length; i++) {
                move = this.previousSeriesStack[i];
                keep = this.previousSeriesStack[i-1];
                serelem = this.series[move].canvas._elem.detach();
                shadelem = this.series[move].shadowCanvas._elem.detach();
                this.series[keep].shadowCanvas._elem.after(shadelem);
                this.series[keep].canvas._elem.after(serelem);
            }
            temp = this.seriesStack.slice(0);
            this.seriesStack = this.previousSeriesStack.slice(0);
            this.previousSeriesStack = temp;
        };
        
        // method: restoreOriginalSeriesOrder
        // This method requires jQuery 1.4+
        // Restore the series canvas order to its original order
        // when the plot was created.
        this.restoreOriginalSeriesOrder = function () {
            var i, j, arr=[];
            for (i=0; i<this.series.length; i++) {
                arr.push(i);
            }
            if (this.seriesStack == arr) {
                return;
            }
            this.previousSeriesStack = this.seriesStack.slice(0);
            this.seriesStack = arr;
            for (i=1; i<this.seriesStack.length; i++) {
                serelem = this.series[i].canvas._elem.detach();
                shadelem = this.series[i].shadowCanvas._elem.detach();
                this.series[i-1].shadowCanvas._elem.after(shadelem);
                this.series[i-1].canvas._elem.after(serelem);
            }
        };
        
        this.activateTheme = function (name) {
            this.themeEngine.activate(this, name);
        };
    }
    
    
    // conpute a highlight color or array of highlight colors from given colors.
    $.jqplot.computeHighlightColors  = function(colors) {
        var ret;
        if (typeof(colors) == "array") {
            ret = [];
            for (var i=0; i<colors.length; i++){
                var rgba = $.jqplot.getColorComponents(colors[i]);
                var newrgb = [rgba[0], rgba[1], rgba[2]];
                var sum = newrgb[0] + newrgb[1] + newrgb[2];
                for (var j=0; j<3; j++) {
                    // when darkening, lowest color component can be is 60.
                    newrgb[j] = (sum > 570) ?  newrgb[j] * 0.8 : newrgb[j] + 0.3 * (255 - newrgb[j]);
                    newrgb[j] = parseInt(newrgb[j], 10);
                }
                ret.push('rgb('+newrgb[0]+','+newrgb[1]+','+newrgb[2]+')');
            }
        }
        else {
            var rgba = $.jqplot.getColorComponents(colors);
            var newrgb = [rgba[0], rgba[1], rgba[2]];
            var sum = newrgb[0] + newrgb[1] + newrgb[2];
            for (var j=0; j<3; j++) {
                // when darkening, lowest color component can be is 60.
                newrgb[j] = (sum > 570) ?  newrgb[j] * 0.8 : newrgb[j] + 0.3 * (255 - newrgb[j]);
                newrgb[j] = parseInt(newrgb[j], 10);
            }
            ret = 'rgb('+newrgb[0]+','+newrgb[1]+','+newrgb[2]+')';
        }
        return ret;
    };
        
   $.jqplot.ColorGenerator = function(colors) {
        var idx = 0;
        
        this.next = function () { 
            if (idx < colors.length) {
                return colors[idx++];
            }
            else {
                idx = 0;
                return colors[idx++];
            }
        };
        
        this.previous = function () { 
            if (idx > 0) {
                return colors[idx--];
            }
            else {
                idx = colors.length-1;
                return colors[idx];
            }
        };
        
        // get a color by index without advancing pointer.
        this.get = function(i) {
            var idx = i - colors.length * Math.floor(i/colors.length);
            return colors[idx];
        };
        
        this.setColors = function(c) {
            colors = c;
        };
        
        this.reset = function() {
            idx = 0;
        };
    };

    // convert a hex color string to rgb string.
    // h - 3 or 6 character hex string, with or without leading #
    // a - optional alpha
    $.jqplot.hex2rgb = function(h, a) {
        h = h.replace('#', '');
        if (h.length == 3) {
            h = h[0]+h[0]+h[1]+h[1]+h[2]+h[2];
        }
        var rgb;
        rgb = 'rgba('+parseInt(h.slice(0,2), 16)+', '+parseInt(h.slice(2,4), 16)+', '+parseInt(h.slice(4,6), 16);
        if (a) {
            rgb += ', '+a;
        }
        rgb += ')';
        return rgb;
    };
    
    // convert an rgb color spec to a hex spec.  ignore any alpha specification.
    $.jqplot.rgb2hex = function(s) {
        var pat = /rgba?\( *([0-9]{1,3}\.?[0-9]*%?) *, *([0-9]{1,3}\.?[0-9]*%?) *, *([0-9]{1,3}\.?[0-9]*%?) *(?:, *[0-9.]*)?\)/;
        var m = s.match(pat);
        var h = '#';
        for (i=1; i<4; i++) {
            var temp;
            if (m[i].search(/%/) != -1) {
                temp = parseInt(255*m[i]/100, 10).toString(16);
                if (temp.length == 1) {
                    temp = '0'+temp;
                }
            }
            else {
                temp = parseInt(m[i], 10).toString(16);
                if (temp.length == 1) {
                    temp = '0'+temp;
                }
            }
            h += temp;
        }
        return h;
    };
    
    // given a css color spec, return an rgb css color spec
    $.jqplot.normalize2rgb = function(s, a) {
        if (s.search(/^ *rgba?\(/) != -1) {
            return s; 
        }
        else if (s.search(/^ *#?[0-9a-fA-F]?[0-9a-fA-F]/) != -1) {
            return $.jqplot.hex2rgb(s, a);
        }
        else {
            throw 'invalid color spec';
        }
    };
    
    // extract the r, g, b, a color components out of a css color spec.
    $.jqplot.getColorComponents = function(s) {
        // check to see if a color keyword.
        s = $.jqplot.colorKeywordMap[s] || s;
        var rgb = $.jqplot.normalize2rgb(s);
        var pat = /rgba?\( *([0-9]{1,3}\.?[0-9]*%?) *, *([0-9]{1,3}\.?[0-9]*%?) *, *([0-9]{1,3}\.?[0-9]*%?) *,? *([0-9.]* *)?\)/;
        var m = rgb.match(pat);
        var ret = [];
        for (i=1; i<4; i++) {
            if (m[i].search(/%/) != -1) {
                ret[i-1] = parseInt(255*m[i]/100, 10);
            }
            else {
                ret[i-1] = parseInt(m[i], 10);
            }
        }
        ret[3] = parseFloat(m[4]) ? parseFloat(m[4]) : 1.0;
        return ret;
    };
    
    $.jqplot.colorKeywordMap = {aliceblue: 'rgb(240, 248, 255)', antiquewhite: 'rgb(250, 235, 215)', aqua: 'rgb( 0, 255, 255)', aquamarine: 'rgb(127, 255, 212)', azure: 'rgb(240, 255, 255)', beige: 'rgb(245, 245, 220)', bisque: 'rgb(255, 228, 196)', black: 'rgb( 0, 0, 0)', blanchedalmond: 'rgb(255, 235, 205)', blue: 'rgb( 0, 0, 255)', blueviolet: 'rgb(138, 43, 226)', brown: 'rgb(165, 42, 42)', burlywood: 'rgb(222, 184, 135)', cadetblue: 'rgb( 95, 158, 160)', chartreuse: 'rgb(127, 255, 0)', chocolate: 'rgb(210, 105, 30)', coral: 'rgb(255, 127, 80)', cornflowerblue: 'rgb(100, 149, 237)', cornsilk: 'rgb(255, 248, 220)', crimson: 'rgb(220, 20, 60)', cyan: 'rgb( 0, 255, 255)', darkblue: 'rgb( 0, 0, 139)', darkcyan: 'rgb( 0, 139, 139)', darkgoldenrod: 'rgb(184, 134, 11)', darkgray: 'rgb(169, 169, 169)', darkgreen: 'rgb( 0, 100, 0)', darkgrey: 'rgb(169, 169, 169)', darkkhaki: 'rgb(189, 183, 107)', darkmagenta: 'rgb(139, 0, 139)', darkolivegreen: 'rgb( 85, 107, 47)', darkorange: 'rgb(255, 140, 0)', darkorchid: 'rgb(153, 50, 204)', darkred: 'rgb(139, 0, 0)', darksalmon: 'rgb(233, 150, 122)', darkseagreen: 'rgb(143, 188, 143)', darkslateblue: 'rgb( 72, 61, 139)', darkslategray: 'rgb( 47, 79, 79)', darkslategrey: 'rgb( 47, 79, 79)', darkturquoise: 'rgb( 0, 206, 209)', darkviolet: 'rgb(148, 0, 211)', deeppink: 'rgb(255, 20, 147)', deepskyblue: 'rgb( 0, 191, 255)', dimgray: 'rgb(105, 105, 105)', dimgrey: 'rgb(105, 105, 105)', dodgerblue: 'rgb( 30, 144, 255)', firebrick: 'rgb(178, 34, 34)', floralwhite: 'rgb(255, 250, 240)', forestgreen: 'rgb( 34, 139, 34)', fuchsia: 'rgb(255, 0, 255)', gainsboro: 'rgb(220, 220, 220)', ghostwhite: 'rgb(248, 248, 255)', gold: 'rgb(255, 215, 0)', goldenrod: 'rgb(218, 165, 32)', gray: 'rgb(128, 128, 128)', grey: 'rgb(128, 128, 128)', green: 'rgb( 0, 128, 0)', greenyellow: 'rgb(173, 255, 47)', honeydew: 'rgb(240, 255, 240)', hotpink: 'rgb(255, 105, 180)', indianred: 'rgb(205, 92, 92)', indigo: 'rgb( 75, 0, 130)', ivory: 'rgb(255, 255, 240)', khaki: 'rgb(240, 230, 140)', lavender: 'rgb(230, 230, 250)', lavenderblush: 'rgb(255, 240, 245)', lawngreen: 'rgb(124, 252, 0)', lemonchiffon: 'rgb(255, 250, 205)', lightblue: 'rgb(173, 216, 230)', lightcoral: 'rgb(240, 128, 128)', lightcyan: 'rgb(224, 255, 255)', lightgoldenrodyellow: 'rgb(250, 250, 210)', lightgray: 'rgb(211, 211, 211)', lightgreen: 'rgb(144, 238, 144)', lightgrey: 'rgb(211, 211, 211)', lightpink: 'rgb(255, 182, 193)', lightsalmon: 'rgb(255, 160, 122)', lightseagreen: 'rgb( 32, 178, 170)', lightskyblue: 'rgb(135, 206, 250)', lightslategray: 'rgb(119, 136, 153)', lightslategrey: 'rgb(119, 136, 153)', lightsteelblue: 'rgb(176, 196, 222)', lightyellow: 'rgb(255, 255, 224)', lime: 'rgb( 0, 255, 0)', limegreen: 'rgb( 50, 205, 50)', linen: 'rgb(250, 240, 230)', magenta: 'rgb(255, 0, 255)', maroon: 'rgb(128, 0, 0)', mediumaquamarine: 'rgb(102, 205, 170)', mediumblue: 'rgb( 0, 0, 205)', mediumorchid: 'rgb(186, 85, 211)', mediumpurple: 'rgb(147, 112, 219)', mediumseagreen: 'rgb( 60, 179, 113)', mediumslateblue: 'rgb(123, 104, 238)', mediumspringgreen: 'rgb( 0, 250, 154)', mediumturquoise: 'rgb( 72, 209, 204)', mediumvioletred: 'rgb(199, 21, 133)', midnightblue: 'rgb( 25, 25, 112)', mintcream: 'rgb(245, 255, 250)', mistyrose: 'rgb(255, 228, 225)', moccasin: 'rgb(255, 228, 181)', navajowhite: 'rgb(255, 222, 173)', navy: 'rgb( 0, 0, 128)', oldlace: 'rgb(253, 245, 230)', olive: 'rgb(128, 128, 0)', olivedrab: 'rgb(107, 142, 35)', orange: 'rgb(255, 165, 0)', orangered: 'rgb(255, 69, 0)', orchid: 'rgb(218, 112, 214)', palegoldenrod: 'rgb(238, 232, 170)', palegreen: 'rgb(152, 251, 152)', paleturquoise: 'rgb(175, 238, 238)', palevioletred: 'rgb(219, 112, 147)', papayawhip: 'rgb(255, 239, 213)', peachpuff: 'rgb(255, 218, 185)', peru: 'rgb(205, 133, 63)', pink: 'rgb(255, 192, 203)', plum: 'rgb(221, 160, 221)', powderblue: 'rgb(176, 224, 230)', purple: 'rgb(128, 0, 128)', red: 'rgb(255, 0, 0)', rosybrown: 'rgb(188, 143, 143)', royalblue: 'rgb( 65, 105, 225)', saddlebrown: 'rgb(139, 69, 19)', salmon: 'rgb(250, 128, 114)', sandybrown: 'rgb(244, 164, 96)', seagreen: 'rgb( 46, 139, 87)', seashell: 'rgb(255, 245, 238)', sienna: 'rgb(160, 82, 45)', silver: 'rgb(192, 192, 192)', skyblue: 'rgb(135, 206, 235)', slateblue: 'rgb(106, 90, 205)', slategray: 'rgb(112, 128, 144)', slategrey: 'rgb(112, 128, 144)', snow: 'rgb(255, 250, 250)', springgreen: 'rgb( 0, 255, 127)', steelblue: 'rgb( 70, 130, 180)', tan: 'rgb(210, 180, 140)', teal: 'rgb( 0, 128, 128)', thistle: 'rgb(216, 191, 216)', tomato: 'rgb(255, 99, 71)', turquoise: 'rgb( 64, 224, 208)', violet: 'rgb(238, 130, 238)', wheat: 'rgb(245, 222, 179)', white: 'rgb(255, 255, 255)', whitesmoke: 'rgb(245, 245, 245)', yellow: 'rgb(255, 255, 0)', yellowgreen: 'rgb(154, 205, 50)'};
        
    // Convienence function that won't hang IE.
    $.jqplot.log = function() {
        if (window.console && $.jqplot.debug) {
           if (arguments.length == 1) {
               console.log (arguments[0]);
            }
           else {
               console.log(arguments);
            }
        }
    };
    var log = $.jqplot.log;
    

    // class: $.jqplot.AxisLabelRenderer
    // Renderer to place labels on the axes.
    $.jqplot.AxisLabelRenderer = function(options) {
        // Group: Properties
        $.jqplot.ElemContainer.call(this);
        // name of the axis associated with this tick
        this.axis;
        // prop: show
        // wether or not to show the tick (mark and label).
        this.show = true;
        // prop: label
        // The text or html for the label.
        this.label = '';
        this.fontFamily = null;
        this.fontSize = null;
        this.textColor = null;
        this._elem;
        // prop: escapeHTML
        // true to escape HTML entities in the label.
        this.escapeHTML = false;
        
        $.extend(true, this, options);
    };
    
    $.jqplot.AxisLabelRenderer.prototype = new $.jqplot.ElemContainer();
    $.jqplot.AxisLabelRenderer.prototype.constructor = $.jqplot.AxisLabelRenderer;
    
    $.jqplot.AxisLabelRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
    };
    
    $.jqplot.AxisLabelRenderer.prototype.draw = function() {
        this._elem = $('<div style="position:absolute;" class="jqplot-'+this.axis+'-label"></div>');
        
        if (Number(this.label)) {
            this._elem.css('white-space', 'nowrap');
        }
        
        if (!this.escapeHTML) {
            this._elem.html(this.label);
        }
        else {
            this._elem.text(this.label);
        }
        if (this.fontFamily) {
            this._elem.css('font-family', this.fontFamily);
        }
        if (this.fontSize) {
            this._elem.css('font-size', this.fontSize);
        }
        if (this.textColor) {
            this._elem.css('color', this.textColor);
        }
        
        return this._elem;
    };
    
    $.jqplot.AxisLabelRenderer.prototype.pack = function() {
    };

    // class: $.jqplot.AxisTickRenderer
    // A "tick" object showing the value of a tick/gridline on the plot.
    $.jqplot.AxisTickRenderer = function(options) {
        // Group: Properties
        $.jqplot.ElemContainer.call(this);
        // prop: mark
        // tick mark on the axis.  One of 'inside', 'outside', 'cross', '' or null.
        this.mark = 'outside';
        // name of the axis associated with this tick
        this.axis;
        // prop: showMark
        // wether or not to show the mark on the axis.
        this.showMark = true;
        // prop: showGridline
        // wether or not to draw the gridline on the grid at this tick.
        this.showGridline = true;
        // prop: isMinorTick
        // if this is a minor tick.
        this.isMinorTick = false;
        // prop: size
        // Length of the tick beyond the grid in pixels.
        // DEPRECATED: This has been superceeded by markSize
        this.size = 4;
        // prop:  markSize
        // Length of the tick marks in pixels.  For 'cross' style, length
        // will be stoked above and below axis, so total length will be twice this.
        this.markSize = 6;
        // prop: show
        // wether or not to show the tick (mark and label).
        // Setting this to false requires more testing.  It is recommended
        // to set showLabel and showMark to false instead.
        this.show = true;
        // prop: showLabel
        // wether or not to show the label.
        this.showLabel = true;
        this.label = '';
        this.value = null;
        this._styles = {};
        // prop: formatter
        // A class of a formatter for the tick text.  sprintf by default.
        this.formatter = $.jqplot.DefaultTickFormatter;
        // prop: prefix
        // string appended to the tick label if no formatString is specified.
        this.prefix = '';
        // prop: formatString
        // string passed to the formatter.
        this.formatString = '';
        // prop: fontFamily
        // css spec for the font-family css attribute.
        this.fontFamily;
        // prop: fontSize
        // css spec for the font-size css attribute.
        this.fontSize;
        // prop: textColor
        // css spec for the color attribute.
        this.textColor;
        this._elem;
        
        $.extend(true, this, options);
    };
    
    $.jqplot.AxisTickRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
    };
    
    $.jqplot.AxisTickRenderer.prototype = new $.jqplot.ElemContainer();
    $.jqplot.AxisTickRenderer.prototype.constructor = $.jqplot.AxisTickRenderer;
    
    $.jqplot.AxisTickRenderer.prototype.setTick = function(value, axisName, isMinor) {
        this.value = value;
        this.axis = axisName;
        if (isMinor) {
            this.isMinorTick = true;
        }
        return this;
    };
    
    $.jqplot.AxisTickRenderer.prototype.draw = function() {
        if (!this.label) {
            this.label = this.formatter(this.formatString, this.value);
        }
        // add prefix if needed
        if (this.prefix && !this.formatString) {
            this.label = this.prefix + this.label;
        }
        style ='style="position:absolute;';
        if (Number(this.label)) {
            style +='white-space:nowrap;';
        }
        style += '"';
        this._elem = $('<div '+style+' class="jqplot-'+this.axis+'-tick">'+this.label+'</div>');
        for (var s in this._styles) {
            this._elem.css(s, this._styles[s]);
        }
        if (this.fontFamily) {
            this._elem.css('font-family', this.fontFamily);
        }
        if (this.fontSize) {
            this._elem.css('font-size', this.fontSize);
        }
        if (this.textColor) {
            this._elem.css('color', this.textColor);
        }
        return this._elem;
    };
        
    $.jqplot.DefaultTickFormatter = function (format, val) {
        if (typeof val == 'number') {
            if (!format) {
                format = $.jqplot.config.defaultTickFormatString;
            }
            return $.jqplot.sprintf(format, val);
        }
        else {
            return String(val);
        }
    };
    
    $.jqplot.AxisTickRenderer.prototype.pack = function() {
    };
     
    // Class: $.jqplot.CanvasGridRenderer
    // The default jqPlot grid renderer, creating a grid on a canvas element.
    // The renderer has no additional options beyond the <Grid> class.
    $.jqplot.CanvasGridRenderer = function(){
        this.shadowRenderer = new $.jqplot.ShadowRenderer();
    };
    
    // called with context of Grid object
    $.jqplot.CanvasGridRenderer.prototype.init = function(options) {
        this._ctx;
        $.extend(true, this, options);
        // set the shadow renderer options
        var sopts = {lineJoin:'miter', lineCap:'round', fill:false, isarc:false, angle:this.shadowAngle, offset:this.shadowOffset, alpha:this.shadowAlpha, depth:this.shadowDepth, lineWidth:this.shadowWidth, closePath:false, strokeStyle:this.shadowColor};
        this.renderer.shadowRenderer.init(sopts);
    };
    
    // called with context of Grid.
    $.jqplot.CanvasGridRenderer.prototype.createElement = function() {
        var elem = document.createElement('canvas');
        var w = this._plotDimensions.width;
        var h = this._plotDimensions.height;
        elem.width = w;
        elem.height = h;
        this._elem = $(elem);
        this._elem.addClass('jqplot-grid-canvas');
        this._elem.css({ position: 'absolute', left: 0, top: 0 });
        if ($.jqplot.use_excanvas) {
            window.G_vmlCanvasManager.init_(document);
        }
        if ($.jqplot.use_excanvas) {
            elem = window.G_vmlCanvasManager.initElement(elem);
        }
        this._top = this._offsets.top;
        this._bottom = h - this._offsets.bottom;
        this._left = this._offsets.left;
        this._right = w - this._offsets.right;
        this._width = this._right - this._left;
        this._height = this._bottom - this._top;
        return this._elem;
    };
    
    $.jqplot.CanvasGridRenderer.prototype.draw = function() {
        this._ctx = this._elem.get(0).getContext("2d");
        var ctx = this._ctx;
        var axes = this._axes;
        // Add the grid onto the grid canvas.  This is the bottom most layer.
        ctx.save();
        ctx.clearRect(0, 0, this._plotDimensions.width, this._plotDimensions.height);
        ctx.fillStyle = this.backgroundColor || this.background;
        ctx.fillRect(this._left, this._top, this._width, this._height);
        
        if (this.drawGridlines) {
            ctx.save();
            ctx.lineJoin = 'miter';
            ctx.lineCap = 'butt';
            ctx.lineWidth = this.gridLineWidth;
            ctx.strokeStyle = this.gridLineColor;
            var b, e;
            var ax = ['xaxis', 'yaxis', 'x2axis', 'y2axis'];
            for (var i=4; i>0; i--) {
                var name = ax[i-1];
                var axis = axes[name];
                var ticks = axis._ticks;
                if (axis.show) {
                    for (var j=ticks.length; j>0; j--) {
                        var t = ticks[j-1];
                        if (t.show) {
                            var pos = Math.round(axis.u2p(t.value)) + 0.5;
                            switch (name) {
                                case 'xaxis':
                                    // draw the grid line
                                    if (t.showGridline) {
                                        drawLine(pos, this._top, pos, this._bottom);
                                    }
                                    
                                    // draw the mark
                                    if (t.showMark && t.mark) {
                                        s = t.markSize;
                                        m = t.mark;
                                        var pos = Math.round(axis.u2p(t.value)) + 0.5;
                                        switch (m) {
                                            case 'outside':
                                                b = this._bottom;
                                                e = this._bottom+s;
                                                break;
                                            case 'inside':
                                                b = this._bottom-s;
                                                e = this._bottom;
                                                break;
                                            case 'cross':
                                                b = this._bottom-s;
                                                e = this._bottom+s;
                                                break;
                                            default:
                                                b = this._bottom;
                                                e = this._bottom+s;
                                                break;
                                        }
                                        // draw the shadow
                                        if (this.shadow) {
                                            this.renderer.shadowRenderer.draw(ctx, [[pos,b],[pos,e]], {lineCap:'butt', lineWidth:this.gridLineWidth, offset:this.gridLineWidth*0.75, depth:2, fill:false, closePath:false});
                                        }
                                        // draw the line
                                        drawLine(pos, b, pos, e);
                                    }
                                    break;
                                case 'yaxis':
                                    // draw the grid line
                                    if (t.showGridline) {
                                        drawLine(this._right, pos, this._left, pos);
                                    }
                                    // draw the mark
                                    if (t.showMark && t.mark) {
                                        s = t.markSize;
                                        m = t.mark;
                                        var pos = Math.round(axis.u2p(t.value)) + 0.5;
                                        switch (m) {
                                            case 'outside':
                                                b = this._left-s;
                                                e = this._left;
                                                break;
                                            case 'inside':
                                                b = this._left;
                                                e = this._left+s;
                                                break;
                                            case 'cross':
                                                b = this._left-s;
                                                e = this._left+s;
                                                break;
                                            default:
                                                b = this._left-s;
                                                e = this._left;
                                                break;
                                                }
                                        // draw the shadow
                                        if (this.shadow) {
                                            this.renderer.shadowRenderer.draw(ctx, [[b, pos], [e, pos]], {lineCap:'butt', lineWidth:this.gridLineWidth*1.5, offset:this.gridLineWidth*0.75, fill:false, closePath:false});
                                        }
                                        drawLine(b, pos, e, pos, {strokeStyle:axis.borderColor});
                                    }
                                    break;
                                case 'x2axis':
                                    // draw the grid line
                                    if (t.showGridline) {
                                        drawLine(pos, this._bottom, pos, this._top);
                                    }
                                    // draw the mark
                                    if (t.showMark && t.mark) {
                                        s = t.markSize;
                                        m = t.mark;
                                        var pos = Math.round(axis.u2p(t.value)) + 0.5;
                                        switch (m) {
                                            case 'outside':
                                                b = this._top-s;
                                                e = this._top;
                                                break;
                                            case 'inside':
                                                b = this._top;
                                                e = this._top+s;
                                                break;
                                            case 'cross':
                                                b = this._top-s;
                                                e = this._top+s;
                                                break;
                                            default:
                                                b = this._top-s;
                                                e = this._top;
                                                break;
                                                }
                                        // draw the shadow
                                        if (this.shadow) {
                                            this.renderer.shadowRenderer.draw(ctx, [[pos,b],[pos,e]], {lineCap:'butt', lineWidth:this.gridLineWidth, offset:this.gridLineWidth*0.75, depth:2, fill:false, closePath:false});
                                        }
                                        drawLine(pos, b, pos, e);
                                    }
                                    break;
                                case 'y2axis':
                                    // draw the grid line
                                    if (t.showGridline) {
                                        drawLine(this._left, pos, this._right, pos);
                                    }
                                    // draw the mark
                                    if (t.showMark && t.mark) {
                                        s = t.markSize;
                                        m = t.mark;
                                        var pos = Math.round(axis.u2p(t.value)) + 0.5;
                                        switch (m) {
                                            case 'outside':
                                                b = this._right;
                                                e = this._right+s;
                                                break;
                                            case 'inside':
                                                b = this._right-s;
                                                e = this._right;
                                                break;
                                            case 'cross':
                                                b = this._right-s;
                                                e = this._right+s;
                                                break;
                                            default:
                                                b = this._right;
                                                e = this._right+s;
                                                break;
                                                }
                                        // draw the shadow
                                        if (this.shadow) {
                                            this.renderer.shadowRenderer.draw(ctx, [[b, pos], [e, pos]], {lineCap:'butt', lineWidth:this.gridLineWidth*1.5, offset:this.gridLineWidth*0.75, fill:false, closePath:false});
                                        }
                                        drawLine(b, pos, e, pos, {strokeStyle:axis.borderColor});
                                    }
                                    break;
                                default:
                                    break;
                            }
                        }
                    }
                }
            }
            // Now draw grid lines for additional y axes
            ax = ['y3axis', 'y4axis', 'y5axis', 'y6axis', 'y7axis', 'y8axis', 'y9axis'];
            for (var i=7; i>0; i--) {
                var axis = axes[ax[i-1]];
                var ticks = axis._ticks;
                if (axis.show) {
                    var tn = ticks[axis.numberTicks-1];
                    var t0 = ticks[0];
                    var left = axis.getLeft();
                    var points = [[left, tn.getTop() + tn.getHeight()/2], [left, t0.getTop() + t0.getHeight()/2 + 1.0]];
                    // draw the shadow
                    if (this.shadow) {
                        this.renderer.shadowRenderer.draw(ctx, points, {lineCap:'butt', fill:false, closePath:false});
                    }
                    // draw the line
                    drawLine(points[0][0], points[0][1], points[1][0], points[1][1], {lineCap:'butt', strokeStyle:axis.borderColor, lineWidth:axis.borderWidth});
                    // draw the tick marks
                    for (var j=ticks.length; j>0; j--) {
                        var t = ticks[j-1];
                        s = t.markSize;
                        m = t.mark;
                        var pos = Math.round(axis.u2p(t.value)) + 0.5;
                        if (t.showMark && t.mark) {
                            switch (m) {
                                case 'outside':
                                    b = left;
                                    e = left+s;
                                    break;
                                case 'inside':
                                    b = left-s;
                                    e = left;
                                    break;
                                case 'cross':
                                    b = left-s;
                                    e = left+s;
                                    break;
                                default:
                                    b = left;
                                    e = left+s;
                                    break;
                            }
                            points = [[b,pos], [e,pos]];
                            // draw the shadow
                            if (this.shadow) {
                                this.renderer.shadowRenderer.draw(ctx, points, {lineCap:'butt', lineWidth:this.gridLineWidth*1.5, offset:this.gridLineWidth*0.75, fill:false, closePath:false});
                            }
                            // draw the line
                            drawLine(b, pos, e, pos, {strokeStyle:axis.borderColor});
                        }
                    }
                }
            }
            
            ctx.restore();
        }
        
        function drawLine(bx, by, ex, ey, opts) {
            ctx.save();
            opts = opts || {};
            if (opts.lineWidth == null || opts.lineWidth != 0){
                $.extend(true, ctx, opts);
                ctx.beginPath();
                ctx.moveTo(bx, by);
                ctx.lineTo(ex, ey);
                ctx.stroke();
                ctx.restore();
            }
        }
        
        if (this.shadow) {
            var points = [[this._left, this._bottom], [this._right, this._bottom], [this._right, this._top]];
            this.renderer.shadowRenderer.draw(ctx, points);
        }
        // Now draw border around grid.  Use axis border definitions. start at
        // upper left and go clockwise.
        if (this.borderWidth != 0 && this.drawBorder) {
            drawLine (this._left, this._top, this._right, this._top, {lineCap:'round', strokeStyle:axes.x2axis.borderColor, lineWidth:axes.x2axis.borderWidth});
            drawLine (this._right, this._top, this._right, this._bottom, {lineCap:'round', strokeStyle:axes.y2axis.borderColor, lineWidth:axes.y2axis.borderWidth});
            drawLine (this._right, this._bottom, this._left, this._bottom, {lineCap:'round', strokeStyle:axes.xaxis.borderColor, lineWidth:axes.xaxis.borderWidth});
            drawLine (this._left, this._bottom, this._left, this._top, {lineCap:'round', strokeStyle:axes.yaxis.borderColor, lineWidth:axes.yaxis.borderWidth});
        }
        // ctx.lineWidth = this.borderWidth;
        // ctx.strokeStyle = this.borderColor;
        // ctx.strokeRect(this._left, this._top, this._width, this._height);
        
    
        ctx.restore();
    };
   
    /**
     * Date instance methods
     *
     * @author Ken Snyder (ken d snyder at gmail dot com)
     * @date 2008-09-10
     * @version 2.0.2 (http://kendsnyder.com/sandbox/date/)     
     * @license Creative Commons Attribution License 3.0 (http://creativecommons.org/licenses/by/3.0/)
     *
     * @contributions Chris Leonello
     * @comment Bug fix to 12 hour time and additions to handle milliseconds and 
     * @comment 24 hour time without am/pm suffix
     *
     */
 
    // begin by creating a scope for utility variables
    
    //
    // pre-calculate the number of milliseconds in a day
    //  
    
    var day = 24 * 60 * 60 * 1000;
    //
    // function to add leading zeros
    //
    var zeroPad = function(number, digits) {
        number = String(number);
        while (number.length < digits) {
            number = '0' + number;
        }
        return number;
    };
    //
    // set up integers and functions for adding to a date or subtracting two dates
    //
    var multipliers = {
        millisecond: 1,
        second: 1000,
        minute: 60 * 1000,
        hour: 60 * 60 * 1000,
        day: day,
        week: 7 * day,
        month: {
            // add a number of months
            add: function(d, number) {
                // add any years needed (increments of 12)
                multipliers.year.add(d, Math[number > 0 ? 'floor' : 'ceil'](number / 12));
                // ensure that we properly wrap betwen December and January
                var prevMonth = d.getMonth() + (number % 12);
                if (prevMonth == 12) {
                    prevMonth = 0;
                    d.setYear(d.getFullYear() + 1);
                } else if (prevMonth == -1) {
                    prevMonth = 11;
                    d.setYear(d.getFullYear() - 1);
                }
                d.setMonth(prevMonth);
            },
            // get the number of months between two Date objects (decimal to the nearest day)
            diff: function(d1, d2) {
                // get the number of years
                var diffYears = d1.getFullYear() - d2.getFullYear();
                // get the number of remaining months
                var diffMonths = d1.getMonth() - d2.getMonth() + (diffYears * 12);
                // get the number of remaining days
                var diffDays = d1.getDate() - d2.getDate();
                // return the month difference with the days difference as a decimal
                return diffMonths + (diffDays / 30);
            }
        },
        year: {
            // add a number of years
            add: function(d, number) {
                d.setYear(d.getFullYear() + Math[number > 0 ? 'floor' : 'ceil'](number));
            },
            // get the number of years between two Date objects (decimal to the nearest day)
            diff: function(d1, d2) {
                return multipliers.month.diff(d1, d2) / 12;
            }
        }        
    };
    //
    // alias each multiplier with an 's' to allow 'year' and 'years' for example
    //
    for (var unit in multipliers) {
        if (unit.substring(unit.length - 1) != 's') { // IE will iterate newly added properties :|
            multipliers[unit + 's'] = multipliers[unit];
        }
    }
    //
    // take a date instance and a format code and return the formatted value
    //
    var format = function(d, code) {
            if (Date.prototype.strftime.formatShortcuts[code]) {
                    // process any shortcuts recursively
                    return d.strftime(Date.prototype.strftime.formatShortcuts[code]);
            } else {
                    // get the format code function and toPaddedString() argument
                    var getter = (Date.prototype.strftime.formatCodes[code] || '').split('.');
                    var nbr = d['get' + getter[0]] ? d['get' + getter[0]]() : '';
                    // run toPaddedString() if specified
                    if (getter[1]) {
                        nbr = zeroPad(nbr, getter[1]);
                    }
                    // prepend the leading character
                    return nbr;
            }       
    };
    //
    // Add methods to Date instances
    //
    var instanceMethods = {
        //
        // Return a date one day ahead (or any other unit)
        //
        // @param string unit
        // units: year | month | day | week | hour | minute | second | millisecond
        // @return object Date
        //
        succ: function(unit) {
            return this.clone().add(1, unit);
        },
        //
        // Add an arbitrary amount to the currently stored date
        //
        // @param integer/float number      
        // @param string unit
        // @return object Date (chainable)      
        //
        add: function(number, unit) {
            var factor = multipliers[unit] || multipliers.day;
            if (typeof factor == 'number') {
                this.setTime(this.getTime() + (factor * number));
            } else {
                factor.add(this, number);
            }
            return this;
        },
        //
        // Find the difference between the current and another date
        //
        // @param string/object dateObj
        // @param string unit
        // @param boolean allowDecimal
        // @return integer/float
        //
        diff: function(dateObj, unit, allowDecimal) {
            // ensure we have a Date object
            dateObj = Date.create(dateObj);
            if (dateObj === null) {
                return null;
            }
            // get the multiplying factor integer or factor function
            var factor = multipliers[unit] || multipliers.day;
            if (typeof factor == 'number') {
                // multiply
                var unitDiff = (this.getTime() - dateObj.getTime()) / factor;
            } else {
                // run function
                var unitDiff = factor.diff(this, dateObj);
            }
            // if decimals are not allowed, round toward zero
            return (allowDecimal ? unitDiff : Math[unitDiff > 0 ? 'floor' : 'ceil'](unitDiff));          
        },
        //
        // Convert a date to a string using traditional strftime format codes
        //
        // @param string formatStr
        // @return string
        //
        strftime: function(formatStr) {
            // default the format string to year-month-day
            var source = formatStr || '%Y-%m-%d', result = '', match;
            // Account for display of time in local time or as UTC time
            // var val = ($.jqplot.comfig.convertUTCtoLocaltime) ? this : 
            // replace each format code
            while (source.length > 0) {
                if (match = source.match(Date.prototype.strftime.formatCodes.matcher)) {
                    result += source.slice(0, match.index);
                    result += (match[1] || '') + format(this, match[2]);
                    source = source.slice(match.index + match[0].length);
                } else {
                    result += source;
                    source = '';
                }
            }
            return result;
        },
        //
        // Return a proper two-digit year integer
        //
        // @return integer
        //
        getShortYear: function() {
            return this.getYear() % 100;
        },
        //
        // Get the number of the current month, 1-12
        //
        // @return integer
        //
        getMonthNumber: function() {
            return this.getMonth() + 1;
        },
        //
        // Get the name of the current month
        //
        // @return string
        //
        getMonthName: function() {
            return Date.MONTHNAMES[this.getMonth()];
        },
        //
        // Get the abbreviated name of the current month
        //
        // @return string
        //
        getAbbrMonthName: function() {
            return Date.ABBR_MONTHNAMES[this.getMonth()];
        },
        //
        // Get the name of the current week day
        //
        // @return string
        //      
        getDayName: function() {
            return Date.DAYNAMES[this.getDay()];
        },
        //
        // Get the abbreviated name of the current week day
        //
        // @return string
        //      
        getAbbrDayName: function() {
            return Date.ABBR_DAYNAMES[this.getDay()];
        },
        //
        // Get the ordinal string associated with the day of the month (i.e. st, nd, rd, th)
        //
        // @return string
        //      
        getDayOrdinal: function() {
            return Date.ORDINALNAMES[this.getDate() % 10];
        },
        //
        // Get the current hour on a 12-hour scheme
        //
        // @return integer
        //
        getHours12: function() {
            var hours = this.getHours();
            return hours > 12 ? hours - 12 : (hours == 0 ? 12 : hours);
        },
        //
        // Get the AM or PM for the current time
        //
        // @return string
        //
        getAmPm: function() {
            return this.getHours() >= 12 ? 'PM' : 'AM';
        },
        //
        // Get the current date as a Unix timestamp
        //
        // @return integer
        //
        getUnix: function() {
            return Math.round(this.getTime() / 1000, 0);
        },
        //
        // Get the GMT offset in hours and minutes (e.g. +06:30)
        //
        // @return string
        //
        getGmtOffset: function() {
            // divide the minutes offset by 60
            var hours = this.getTimezoneOffset() / 60;
            // decide if we are ahead of or behind GMT
            var prefix = hours < 0 ? '+' : '-';
            // remove the negative sign if any
            hours = Math.abs(hours);
            // add the +/- to the padded number of hours to : to the padded minutes
            return prefix + zeroPad(Math.floor(hours), 2) + ':' + zeroPad((hours % 1) * 60, 2);
        },
        //
        // Get the browser-reported name for the current timezone (e.g. MDT, Mountain Daylight Time)
        //
        // @return string
        //
        getTimezoneName: function() {
            var match = /(?:\((.+)\)$| ([A-Z]{3}) )/.exec(this.toString());
            return match[1] || match[2] || 'GMT' + this.getGmtOffset();
        },
        //
        // Convert the current date to an 8-digit integer (%Y%m%d)
        //
        // @return int
        //
        toYmdInt: function() {
            return (this.getFullYear() * 10000) + (this.getMonthNumber() * 100) + this.getDate();
        },  
        //
        // Create a copy of a date object
        //
        // @return object
        //       
        clone: function() {
                return new Date(this.getTime());
        }
    };
    for (var name in instanceMethods) {
        Date.prototype[name] = instanceMethods[name];
    }
    //
    // Add static methods to the date object
    //
    var staticMethods = {
        //
        // The heart of the date functionality: returns a date object if given a convertable value
        //
        // @param string/object/integer date
        // @return object Date
        //
        create: function(date) {
            // If the passed value is already a date object, return it
            if (date instanceof Date) {
                return date;
            }
            // if (typeof date == 'number') return new Date(date);
            // If the passed value is an integer, interpret it as a javascript timestamp
            if (typeof date == 'number') {
                return new Date(date);
            }
            // If the passed value is a string, attempt to parse it using Date.parse()
            var parsable = String(date).replace(/^\s*(.+)\s*$/, '$1'), i = 0, length = Date.create.patterns.length, pattern;
            var current = parsable;
            while (i < length) {
                ms = Date.parse(current);
                if (!isNaN(ms)) {
                    return new Date(ms);
                }
                pattern = Date.create.patterns[i];
                if (typeof pattern == 'function') {
                    obj = pattern(current);
                    if (obj instanceof Date) {
                        return obj;
                    }
                } else {
                    current = parsable.replace(pattern[0], pattern[1]);
                }
                i++;
            }
            return NaN;
        },
        //
        // constants representing month names, day names, and ordinal names
        // (same names as Ruby Date constants)
        //
        MONTHNAMES          : 'January February March April May June July August September October November December'.split(' '),
        ABBR_MONTHNAMES : 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'.split(' '),
        DAYNAMES                : 'Sunday Monday Tuesday Wednesday Thursday Friday Saturday'.split(' '),
        ABBR_DAYNAMES       : 'Sun Mon Tue Wed Thu Fri Sat'.split(' '),
        ORDINALNAMES        : 'th st nd rd th th th th th th'.split(' '),
        //
        // Shortcut for full ISO-8601 date conversion
        //
        ISO: '%Y-%m-%dT%H:%M:%S.%N%G',
        //
        // Shortcut for SQL-type formatting
        //
        SQL: '%Y-%m-%d %H:%M:%S',
        //
        // Setter method for month, day, and ordinal names for i18n
        //
        // @param object newNames
        //
        daysInMonth: function(year, month) {
            if (month == 2) {
                return new Date(year, 1, 29).getDate() == 29 ? 29 : 28;
            }
            return [undefined,31,undefined,31,30,31,30,31,31,30,31,30,31][month];
        }
    };
    for (var name in staticMethods) {
        Date[name] = staticMethods[name];
    }
    //
    // format codes for strftime
    //
    // each code must be an array where the first member is the name of a Date.prototype function
    // and optionally a second member indicating the number to pass to Number#toPaddedString()
    //
    Date.prototype.strftime.formatCodes = {
        //
        // 2-part regex matcher for format codes
        //
        // first match must be the character before the code (to account for escaping)
        // second match must be the format code character(s)
        //
        matcher: /()%(#?(%|[a-z]))/i,
        // year
        Y: 'FullYear',
        y: 'ShortYear.2',
        // month
        m: 'MonthNumber.2',
        '#m': 'MonthNumber',
        B: 'MonthName',
        b: 'AbbrMonthName',
        // day
        d: 'Date.2',
        '#d': 'Date',
        e: 'Date',
        A: 'DayName',
        a: 'AbbrDayName',
        w: 'Day',
        o: 'DayOrdinal',
        // hours
        H: 'Hours.2',
        '#H': 'Hours',
        I: 'Hours12.2',
        '#I': 'Hours12',
        p: 'AmPm',
        // minutes
        M: 'Minutes.2',
        '#M': 'Minutes',
        // seconds
        S: 'Seconds.2',
        '#S': 'Seconds',
        s: 'Unix',
        // milliseconds
        N: 'Milliseconds.3',
        '#N': 'Milliseconds',
        // timezone
        O: 'TimezoneOffset',
        Z: 'TimezoneName',
        G: 'GmtOffset'  
    };
    //
    // shortcuts that will be translated into their longer version
    //
    // be sure that format shortcuts do not refer to themselves: this will cause an infinite loop
    //
    Date.prototype.strftime.formatShortcuts = {
        // date
        F: '%Y-%m-%d',
        // time
        T: '%H:%M:%S',
        X: '%H:%M:%S',
        // local format date
        x: '%m/%d/%y',
        D: '%m/%d/%y',
        // local format extended
        '#c': '%a %b %e %H:%M:%S %Y',
        // local format short
        v: '%e-%b-%Y',
        R: '%H:%M',
        r: '%I:%M:%S %p',
        // tab and newline
        t: '\t',
        n: '\n',
        '%': '%'
    };
    //
    // A list of conversion patterns (array arguments sent directly to gsub)
    // Add, remove or splice a patterns to customize date parsing ability
    //
    Date.create.patterns = [
        [/-/g, '/'], // US-style time with dashes => Parsable US-style time
        [/st|nd|rd|th/g, ''], // remove st, nd, rd and th        
        [/(3[01]|[0-2]\d)\s*\.\s*(1[0-2]|0\d)\s*\.\s*([1-9]\d{3})/, '$2/$1/$3'], // World time => Parsable US-style time
        [/([1-9]\d{3})\s*-\s*(1[0-2]|0\d)\s*-\s*(3[01]|[0-2]\d)/, '$2/$3/$1'], // ISO-style time => Parsable US-style time
        function(str) { // 12-hour or 24 hour time with milliseconds
            // var match = str.match(/^(?:(.+)\s+)?([1-9]|1[012])(?:\s*\:\s*(\d\d))?(?:\s*\:\s*(\d\d))?\s*(am|pm)\s*$/i);
            var match = str.match(/^(?:(.+)\s+)?([012]?\d)(?:\s*\:\s*(\d\d))?(?:\s*\:\s*(\d\d(\.\d*)?))?\s*(am|pm)?\s*$/i);
            //                   opt. date      hour       opt. minute     opt. second       opt. msec   opt. am or pm
            if (match) {
                if (match[1]) {
                    var d = Date.create(match[1]);
                    if (isNaN(d)) {
                        return;
                    }
                } else {
                    var d = new Date();
                    d.setMilliseconds(0);
                }
                var hour = parseFloat(match[2]);
                if (match[6]) {
                    hour = match[6].toLowerCase() == 'am' ? (hour == 12 ? 0 : hour) : (hour == 12 ? 12 : hour + 12);
                }
                d.setHours(hour, parseInt(match[3] || 0, 10), parseInt(match[4] || 0, 10), ((parseFloat(match[5] || 0)) || 0)*1000);
                return d;
            }
            else {
                return str;
            }
        },
        function(str) { // ISO timestamp with time zone.
            var match = str.match(/^(?:(.+))[T|\s+]([012]\d)(?:\:(\d\d))(?:\:(\d\d))(?:\.\d+)([\+\-]\d\d\:\d\d)$/i);
            if (match) {
                if (match[1]) {
                    var d = Date.create(match[1]);
                    if (isNaN(d)) {
                        return;
                    }
                } else {
                    var d = new Date();
                    d.setMilliseconds(0);
                }
                var hour = parseFloat(match[2]);
                d.setHours(hour, parseInt(match[3], 10), parseInt(match[4], 10), parseFloat(match[5])*1000);
                return d;
            }
            else {
                    return str;
            }
        },
        function(str) {
            var match = str.match(/^([0-3]?\d)\s*[-\/.\s]{1}\s*([a-zA-Z]{3,9})\s*[-\/.\s]{1}\s*([0-3]?\d)$/);
            if (match) {
                var d = new Date();
                var y = parseFloat(String(d.getFullYear()).slice(2,4));
                var cent = parseInt(String(d.getFullYear())/100, 10)*100;
                var centoffset = 1;
                var m1 = parseFloat(match[1]);
                var m3 = parseFloat(match[3]);
                var ny, nd, nm;
                if (m1 > 31) { // first number is a year
                    nd = match[3];
                    if (m1 < y+centoffset) { // if less than 1 year out, assume it is this century.
                        ny = cent + m1;
                    }
                    else {
                        ny = cent - 100 + m1;
                    }
                }
                
                else { // last number is the year
                    nd = match[1];
                    if (m3 < y+centoffset) { // if less than 1 year out, assume it is this century.
                        ny = cent + m3;
                    }
                    else {
                        ny = cent - 100 + m3;
                    }
                }
                
                var nm = $.inArray(match[2], Date.ABBR_MONTHNAMES);
                
                if (nm == -1) {
                    nm = $.inArray(match[2], Date.MONTHNAMES);
                }
            
                d.setFullYear(ny, nm, nd);
                d.setHours(0,0,0,0);
                return d;
            }
            
            else {
                return str;
            }
        }        
    ];
    
    if ($.jqplot.config.debug) {
        $.date = Date.create;
    }

    // Class: $.jqplot.DivTitleRenderer
    // The default title renderer for jqPlot.  This class has no options beyond the <Title> class. 
    $.jqplot.DivTitleRenderer = function() {
    };
    
    $.jqplot.DivTitleRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
    };
    
    $.jqplot.DivTitleRenderer.prototype.draw = function() {
        var r = this.renderer;
        if (!this.text) {
            this.show = false;
            this._elem = $('<div class="jqplot-title" style="height:0px;width:0px;"></div>');
        }
        else if (this.text) {
            var color;
            if (this.color) {
                color = this.color;
            }
            else if (this.textColor) {
                color = this.textColor;
            }
            // don't trust that a stylesheet is present, set the position.
            var styletext = 'position:absolute;top:0px;left:0px;';
            styletext += (this._plotWidth) ? 'width:'+this._plotWidth+'px;' : '';
            styletext += (this.fontSize) ? 'font-size:'+this.fontSize+';' : '';
            styletext += (this.textAlign) ? 'text-align:'+this.textAlign+';' : 'text-align:center;';
            styletext += (color) ? 'color:'+color+';' : '';
            styletext += (this.paddingBottom) ? 'padding-bottom:'+this.paddingBottom+';' : '';
            this._elem = $('<div class="jqplot-title" style="'+styletext+'">'+this.text+'</div>');
            if (this.fontFamily) {
                this._elem.css('font-family', this.fontFamily);
            }
        }
        
        return this._elem;
    };
    
    $.jqplot.DivTitleRenderer.prototype.pack = function() {
        // nothing to do here
    };
  
    // Class: $.jqplot.LineRenderer
    // The default line renderer for jqPlot, this class has no options beyond the <Series> class.
    // Draws series as a line.
    $.jqplot.LineRenderer = function(){
        this.shapeRenderer = new $.jqplot.ShapeRenderer();
        this.shadowRenderer = new $.jqplot.ShadowRenderer();
    };
    
    // called with scope of series.
    $.jqplot.LineRenderer.prototype.init = function(options, plot) {
        options = options || {};
        var lopts = {highlightMouseOver: options.highlightMouseOver, highlightMouseDown: options.highlightMouseDown, highlightColor: options.highlightColor};
        
        delete (options.highlightMouseOver);
        delete (options.highlightMouseDown);
        delete (options.highlightColor);
        
        $.extend(true, this.renderer, options);
        // set the shape renderer options
        var opts = {lineJoin:'round', lineCap:'round', fill:this.fill, isarc:false, strokeStyle:this.color, fillStyle:this.fillColor, lineWidth:this.lineWidth, closePath:this.fill};
        this.renderer.shapeRenderer.init(opts);
        // set the shadow renderer options
        // scale the shadowOffset to the width of the line.
        if (this.lineWidth > 2.5) {
            var shadow_offset = this.shadowOffset* (1 + (Math.atan((this.lineWidth/2.5))/0.785398163 - 1)*0.6);
            // var shadow_offset = this.shadowOffset;
        }
        // for skinny lines, don't make such a big shadow.
        else {
            var shadow_offset = this.shadowOffset*Math.atan((this.lineWidth/2.5))/0.785398163;
        }
        var sopts = {lineJoin:'round', lineCap:'round', fill:this.fill, isarc:false, angle:this.shadowAngle, offset:shadow_offset, alpha:this.shadowAlpha, depth:this.shadowDepth, lineWidth:this.lineWidth, closePath:this.fill};
        this.renderer.shadowRenderer.init(sopts);
        this._areaPoints = [];
        this._boundingBox = [[],[]];
        
        if (!this.isTrendline && this.fill) {
        
            // prop: highlightMouseOver
            // True to highlight area on a filled plot when moused over.
            // This must be false to enable highlightMouseDown to highlight when clicking on an area on a filled plot.
            this.highlightMouseOver = true;
            // prop: highlightMouseDown
            // True to highlight when a mouse button is pressed over an area on a filled plot.
            // This will be disabled if highlightMouseOver is true.
            this.highlightMouseDown = false;
            // prop: highlightColor
            // color to use when highlighting an area on a filled plot.
            this.highlightColor = null;
            // if user has passed in highlightMouseDown option and not set highlightMouseOver, disable highlightMouseOver
            if (lopts.highlightMouseDown && lopts.highlightMouseOver == null) {
                lopts.highlightMouseOver = false;
            }
        
            $.extend(true, this, {highlightMouseOver: lopts.highlightMouseOver, highlightMouseDown: lopts.highlightMouseDown, highlightColor: lopts.highlightColor});
            
            if (!this.highlightColor) {
                this.highlightColor = $.jqplot.computeHighlightColors(this.fillColor);
            }
            // turn off traditional highlighter
            if (this.highlighter) {
                this.highlighter.show = false;
            }
            plot.postInitHooks.addOnce(postInit);
            plot.postDrawHooks.addOnce(postPlotDraw);
            plot.eventListenerHooks.addOnce('jqplotMouseMove', handleMove);
            plot.eventListenerHooks.addOnce('jqplotMouseDown', handleMouseDown);
            plot.eventListenerHooks.addOnce('jqplotMouseUp', handleMouseUp);
            plot.eventListenerHooks.addOnce('jqplotClick', handleClick);
            plot.eventListenerHooks.addOnce('jqplotRightClick', handleRightClick);
        }

    };
    
    // Method: setGridData
    // converts the user data values to grid coordinates and stores them
    // in the gridData array.
    // Called with scope of a series.
    $.jqplot.LineRenderer.prototype.setGridData = function(plot) {
        // recalculate the grid data
        var xp = this._xaxis.series_u2p;
        var yp = this._yaxis.series_u2p;
        var data = this._plotData;
        var pdata = this._prevPlotData;
        this.gridData = [];
        this._prevGridData = [];
        for (var i=0; i<this.data.length; i++) {
            // if not a line series or if no nulls in data, push the converted point onto the array.
            if (data[i][0] != null && data[i][1] != null) {
                this.gridData.push([xp.call(this._xaxis, data[i][0]), yp.call(this._yaxis, data[i][1])]);
            }
            // else if there is a null, preserve it.
            else if (data[i][0] == null) {
                this.gridData.push([null, yp.call(this._yaxis, data[i][1])]);
            }
            else if (data[i][1] == null) {
                this.gridData.push([xp.call(this._xaxis, data[i][0]), null]);
            }
            // if not a line series or if no nulls in data, push the converted point onto the array.
            if (pdata[i] != null && pdata[i][0] != null && pdata[i][1] != null) {
                this._prevGridData.push([xp.call(this._xaxis, pdata[i][0]), yp.call(this._yaxis, pdata[i][1])]);
            }
            // else if there is a null, preserve it.
            else if (pdata[i] != null && pdata[i][0] == null) {
                this._prevGridData.push([null, yp.call(this._yaxis, pdata[i][1])]);
            }  
            else if (pdata[i] != null && pdata[i][0] != null && pdata[i][1] == null) {
                this._prevGridData.push([xp.call(this._xaxis, pdata[i][0]), null]);
            }
        }
    };
    
    // Method: makeGridData
    // converts any arbitrary data values to grid coordinates and
    // returns them.  This method exists so that plugins can use a series'
    // linerenderer to generate grid data points without overwriting the
    // grid data associated with that series.
    // Called with scope of a series.
    $.jqplot.LineRenderer.prototype.makeGridData = function(data, plot) {
        // recalculate the grid data
        var xp = this._xaxis.series_u2p;
        var yp = this._yaxis.series_u2p;
        var gd = [];
        var pgd = [];
        for (var i=0; i<data.length; i++) {
            // if not a line series or if no nulls in data, push the converted point onto the array.
            if (data[i][0] != null && data[i][1] != null) {
                gd.push([xp.call(this._xaxis, data[i][0]), yp.call(this._yaxis, data[i][1])]);
            }
            // else if there is a null, preserve it.
            else if (data[i][0] == null) {
                gd.push([null, yp.call(this._yaxis, data[i][1])]);
            }
            else if (data[i][1] == null) {
                gd.push([xp.call(this._xaxis, data[i][0]), null]);
            }
        }
        return gd;
    };
    

    // called within scope of series.
    $.jqplot.LineRenderer.prototype.draw = function(ctx, gd, options) {
        var i;
        var opts = (options != undefined) ? options : {};
        var shadow = (opts.shadow != undefined) ? opts.shadow : this.shadow;
        var showLine = (opts.showLine != undefined) ? opts.showLine : this.showLine;
        var fill = (opts.fill != undefined) ? opts.fill : this.fill;
        var fillAndStroke = (opts.fillAndStroke != undefined) ? opts.fillAndStroke : this.fillAndStroke;
        var xmin, ymin, xmax, ymax;
        ctx.save();
        if (gd.length) {
            if (showLine) {
                // if we fill, we'll have to add points to close the curve.
                if (fill) {
                    if (this.fillToZero) {
                        // have to break line up into shapes at axis crossings
                        var negativeColors = new $.jqplot.ColorGenerator(this.negativeSeriesColors);
                        var negativeColor = negativeColors.get(this.index);
                        if (! this.useNegativeColors) {
                            negativeColor = opts.fillStyle;
                        }
                        var isnegative = false;
                        var posfs = opts.fillStyle;
                    
                        // if stoking line as well as filling, get a copy of line data.
                        if (fillAndStroke) {
                            var fasgd = gd.slice(0);
                        }
                        // if not stacked, fill down to axis
                        if (this.index == 0 || !this._stack) {
                        
                            var tempgd = [];
                            this._areaPoints = [];
                            var pyzero = this._yaxis.series_u2p(this.fillToValue);
                            var pxzero = this._xaxis.series_u2p(this.fillToValue);
                            
                            if (this.fillAxis == 'y') {
                                tempgd.push([gd[0][0], pyzero]);
                                this._areaPoints.push([gd[0][0], pyzero]);
                                
                                for (var i=0; i<gd.length-1; i++) {
                                    tempgd.push(gd[i]);
                                    this._areaPoints.push(gd[i]);
                                    // do we have an axis crossing?
                                    if (this._plotData[i][1] * this._plotData[i+1][1] < 0) {
                                        if (this._plotData[i][1] < 0) {
                                            isnegative = true;
                                            opts.fillStyle = negativeColor;
                                        }
                                        else {
                                            isnegative = false;
                                            opts.fillStyle = posfs;
                                        }
                                        
                                        var xintercept = gd[i][0] + (gd[i+1][0] - gd[i][0]) * (pyzero-gd[i][1])/(gd[i+1][1] - gd[i][1]);
                                        tempgd.push([xintercept, pyzero]);
                                        this._areaPoints.push([xintercept, pyzero]);
                                        // now draw this shape and shadow.
                                        if (shadow) {
                                            this.renderer.shadowRenderer.draw(ctx, tempgd, opts);
                                        }
                                        this.renderer.shapeRenderer.draw(ctx, tempgd, opts);
                                        // now empty temp array and continue
                                        tempgd = [[xintercept, pyzero]];
                                        // this._areaPoints = [[xintercept, pyzero]];
                                    }   
                                }
                                if (this._plotData[gd.length-1][1] < 0) {
                                    isnegative = true;
                                    opts.fillStyle = negativeColor;
                                }
                                else {
                                    isnegative = false;
                                    opts.fillStyle = posfs;
                                }
                                tempgd.push(gd[gd.length-1]);
                                this._areaPoints.push(gd[gd.length-1]);
                                tempgd.push([gd[gd.length-1][0], pyzero]); 
                                this._areaPoints.push([gd[gd.length-1][0], pyzero]); 
                            }
                            // now draw this shape and shadow.
                            if (shadow) {
                                this.renderer.shadowRenderer.draw(ctx, tempgd, opts);
                            }
                            this.renderer.shapeRenderer.draw(ctx, tempgd, opts);
                            
                            
                            // var gridymin = this._yaxis.series_u2p(0);
                            // // IE doesn't return new length on unshift
                            // gd.unshift([gd[0][0], gridymin]);
                            // len = gd.length;
                            // gd.push([gd[len - 1][0], gridymin]);                   
                        }
                        // if stacked, fill to line below 
                        else {
                            var prev = this._prevGridData;
                            for (var i=prev.length; i>0; i--) {
                                gd.push(prev[i-1]);
                                // this._areaPoints.push(prev[i-1]);
                            }
                            if (shadow) {
                                this.renderer.shadowRenderer.draw(ctx, gd, opts);
                            }
                            this._areaPoints = gd;
                            this.renderer.shapeRenderer.draw(ctx, gd, opts);
                        }
                    }
                    /////////////////////////
                    // Not filled to zero
                    ////////////////////////
                    else {                    
                        // if stoking line as well as filling, get a copy of line data.
                        if (fillAndStroke) {
                            var fasgd = gd.slice(0);
                        }
                        // if not stacked, fill down to axis
                        if (this.index == 0 || !this._stack) {
                            // var gridymin = this._yaxis.series_u2p(this._yaxis.min) - this.gridBorderWidth / 2;
                            var gridymin = ctx.canvas.height;
                            // IE doesn't return new length on unshift
                            gd.unshift([gd[0][0], gridymin]);
                            len = gd.length;
                            gd.push([gd[len - 1][0], gridymin]);                   
                        }
                        // if stacked, fill to line below 
                        else {
                            var prev = this._prevGridData;
                            for (var i=prev.length; i>0; i--) {
                                gd.push(prev[i-1]);
                            }
                        }
                        this._areaPoints = gd;
                        
                        if (shadow) {
                            this.renderer.shadowRenderer.draw(ctx, gd, opts);
                        }
            
                        this.renderer.shapeRenderer.draw(ctx, gd, opts);                        
                    }
                    if (fillAndStroke) {
                        var fasopts = $.extend(true, {}, opts, {fill:false, closePath:false});
                        this.renderer.shapeRenderer.draw(ctx, fasgd, fasopts);
                        //////////
                        // TODO: figure out some way to do shadows nicely
                        // if (shadow) {
                        //     this.renderer.shadowRenderer.draw(ctx, fasgd, fasopts);
                        // }
                        // now draw the markers
                        if (this.markerRenderer.show) {
                            for (i=0; i<fasgd.length; i++) {
                                this.markerRenderer.draw(fasgd[i][0], fasgd[i][1], ctx, opts.markerOptions);
                            }
                        }
                    }
                }
                else {
                    if (shadow) {
                        this.renderer.shadowRenderer.draw(ctx, gd, opts);
                    }
    
                    this.renderer.shapeRenderer.draw(ctx, gd, opts);
                }
            }
            // calculate the bounding box
            var xmin = xmax = ymin = ymax = null;
            for (i=0; i<this._areaPoints.length; i++) {
                var p = this._areaPoints[i];
                if (xmin > p[0] || xmin == null) {
                    xmin = p[0];
                }
                if (ymax < p[1] || ymax == null) {
                    ymax = p[1];
                }
                if (xmax < p[0] || xmax == null) {
                    xmax = p[0];
                }
                if (ymin > p[1] || ymin == null) {
                    ymin = p[1];
                }
            }
            this._boundingBox = [[xmin, ymax], [xmax, ymin]];
        
            // now draw the markers
            if (this.markerRenderer.show && !fill) {
                for (i=0; i<gd.length; i++) {
                    if (gd[i][0] != null && gd[i][1] != null) {
                        this.markerRenderer.draw(gd[i][0], gd[i][1], ctx, opts.markerOptions);
                    }
                }
            }
        }
        
        ctx.restore();
    };  
    
    $.jqplot.LineRenderer.prototype.drawShadow = function(ctx, gd, options) {
        // This is a no-op, shadows drawn with lines.
    };
    
    // called with scope of plot.
    // make sure to not leave anything highlighted.
    function postInit(target, data, options) {
        for (i=0; i<this.series.length; i++) {
            if (this.series[i].renderer.constructor == $.jqplot.LineRenderer) {
                // don't allow mouseover and mousedown at same time.
                if (this.series[i].highlightMouseOver) {
                    this.series[i].highlightMouseDown = false;
                }
            }
        }
        this.target.bind('mouseout', {plot:this}, function (ev) { unhighlight(ev.data.plot); });
    }  
    
    // called within context of plot
    // create a canvas which we can draw on.
    // insert it before the eventCanvas, so eventCanvas will still capture events.
    function postPlotDraw() {
        this.plugins.lineRenderer = {highlightedSeriesIndex:null};
        this.plugins.lineRenderer.highlightCanvas = new $.jqplot.GenericCanvas();
        
        this.eventCanvas._elem.before(this.plugins.lineRenderer.highlightCanvas.createElement(this._gridPadding, 'jqplot-lineRenderer-highlight-canvas', this._plotDimensions));
        var hctx = this.plugins.lineRenderer.highlightCanvas.setContext();
    } 
    
    function highlight (plot, sidx, pidx, points) {
        var s = plot.series[sidx];
        var canvas = plot.plugins.lineRenderer.highlightCanvas;
        canvas._ctx.clearRect(0,0,canvas._ctx.canvas.width, canvas._ctx.canvas.height);
        s._highlightedPoint = pidx;
        plot.plugins.lineRenderer.highlightedSeriesIndex = sidx;
        var opts = {fillStyle: s.highlightColor};
        s.renderer.shapeRenderer.draw(canvas._ctx, points, opts);
    }
    
    function unhighlight (plot) {
        var canvas = plot.plugins.lineRenderer.highlightCanvas;
        canvas._ctx.clearRect(0,0, canvas._ctx.canvas.width, canvas._ctx.canvas.height);
        for (var i=0; i<plot.series.length; i++) {
            plot.series[i]._highlightedPoint = null;
        }
        plot.plugins.lineRenderer.highlightedSeriesIndex = null;
        plot.target.trigger('jqplotDataUnhighlight');
    }
    
    
    function handleMove(ev, gridpos, datapos, neighbor, plot) {
        if (neighbor) {
            var ins = [neighbor.seriesIndex, neighbor.pointIndex, neighbor.data];
            var evt1 = jQuery.Event('jqplotDataMouseOver');
            evt1.pageX = ev.pageX;
            evt1.pageY = ev.pageY;
            plot.target.trigger(evt1, ins);
            if (plot.series[ins[0]].highlightMouseOver && !(ins[0] == plot.plugins.lineRenderer.highlightedSeriesIndex)) {
                var evt = jQuery.Event('jqplotDataHighlight');
                evt.pageX = ev.pageX;
                evt.pageY = ev.pageY;
                plot.target.trigger(evt, ins);
                highlight (plot, neighbor.seriesIndex, neighbor.pointIndex, neighbor.points);
            }
        }
        else if (neighbor == null) {
            unhighlight (plot);
        }
    }
    
    function handleMouseDown(ev, gridpos, datapos, neighbor, plot) {
        if (neighbor) {
            var ins = [neighbor.seriesIndex, neighbor.pointIndex, neighbor.data];
            if (plot.series[ins[0]].highlightMouseDown && !(ins[0] == plot.plugins.lineRenderer.highlightedSeriesIndex)) {
                var evt = jQuery.Event('jqplotDataHighlight');
                evt.pageX = ev.pageX;
                evt.pageY = ev.pageY;
                plot.target.trigger(evt, ins);
                highlight (plot, neighbor.seriesIndex, neighbor.pointIndex, neighbor.points);
            }
        }
        else if (neighbor == null) {
            unhighlight (plot);
        }
    }
    
    function handleMouseUp(ev, gridpos, datapos, neighbor, plot) {
        var idx = plot.plugins.lineRenderer.highlightedSeriesIndex;
        if (idx != null && plot.series[idx].highlightMouseDown) {
            unhighlight(plot);
        }
    }
    
    function handleClick(ev, gridpos, datapos, neighbor, plot) {
        if (neighbor) {
            var ins = [neighbor.seriesIndex, neighbor.pointIndex, neighbor.data];
            var evt = jQuery.Event('jqplotDataClick');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            plot.target.trigger(evt, ins);
        }
    }
    
    function handleRightClick(ev, gridpos, datapos, neighbor, plot) {
        if (neighbor) {
            var ins = [neighbor.seriesIndex, neighbor.pointIndex, neighbor.data];
            var idx = plot.plugins.lineRenderer.highlightedSeriesIndex;
            if (idx != null && plot.series[idx].highlightMouseDown) {
                unhighlight(plot);
            }
            var evt = jQuery.Event('jqplotDataRightClick');
            evt.pageX = ev.pageX;
            evt.pageY = ev.pageY;
            plot.target.trigger(evt, ins);
        }
    }
    
    
    // class: $.jqplot.LinearAxisRenderer
    // The default jqPlot axis renderer, creating a numeric axis.
    // The renderer has no additional options beyond the <Axis> object.
    $.jqplot.LinearAxisRenderer = function() {
    };
    
    // called with scope of axis object.
    $.jqplot.LinearAxisRenderer.prototype.init = function(options){
        $.extend(true, this, options);
        var db = this._dataBounds;
        // Go through all the series attached to this axis and find
        // the min/max bounds for this axis.
        for (var i=0; i<this._series.length; i++) {
            var s = this._series[i];
            var d = s._plotData;
            
            for (var j=0; j<d.length; j++) { 
                if (this.name == 'xaxis' || this.name == 'x2axis') {
                    if ((d[j][0] != null && d[j][0] < db.min) || db.min == null) {
                        db.min = d[j][0];
                    }
                    if ((d[j][0] != null && d[j][0] > db.max) || db.max == null) {
                        db.max = d[j][0];
                    }
                }              
                else {
                    if ((d[j][1] != null && d[j][1] < db.min) || db.min == null) {
                        db.min = d[j][1];
                    }
                    if ((d[j][1] != null && d[j][1] > db.max) || db.max == null) {
                        db.max = d[j][1];
                    }
                }              
            }
        }
    };
    
    // called with scope of axis
    $.jqplot.LinearAxisRenderer.prototype.draw = function(ctx) {
        if (this.show) {
            // populate the axis label and value properties.
            // createTicks is a method on the renderer, but
            // call it within the scope of the axis.
            this.renderer.createTicks.call(this);
            // fill a div with axes labels in the right direction.
            // Need to pregenerate each axis to get it's bounds and
            // position it and the labels correctly on the plot.
            var dim=0;
            var temp;
            // Added for theming.
            if (this._elem) {
                this._elem.empty();
            }
            
            this._elem = $('<div class="jqplot-axis jqplot-'+this.name+'" style="position:absolute;"></div>');
            
            if (this.name == 'xaxis' || this.name == 'x2axis') {
                this._elem.width(this._plotDimensions.width);
            }
            else {
                this._elem.height(this._plotDimensions.height);
            }
            
            // create a _label object.
            this.labelOptions.axis = this.name;
            this._label = new this.labelRenderer(this.labelOptions);
            if (this._label.show) {
                var elem = this._label.draw(ctx);
                elem.appendTo(this._elem);
            }
    
            var t = this._ticks;
            for (var i=0; i<t.length; i++) {
                var tick = t[i];
                if (tick.showLabel && (!tick.isMinorTick || this.showMinorTicks)) {
                    var elem = tick.draw(ctx);
                    elem.appendTo(this._elem);
                }
            }
        }
        return this._elem;
    };
    
    // called with scope of an axis
    $.jqplot.LinearAxisRenderer.prototype.reset = function() {
        this.min = this._min;
        this.max = this._max;
        this.tickInterval = this._tickInterval;
        this.numberTicks = this._numberTicks;
        // this._ticks = this.__ticks;
    };
    
    // called with scope of axis
    $.jqplot.LinearAxisRenderer.prototype.set = function() { 
        var dim = 0;
        var temp;
        var w = 0;
        var h = 0;
        var lshow = (this._label == null) ? false : this._label.show;
        if (this.show) {
            var t = this._ticks;
            for (var i=0; i<t.length; i++) {
                var tick = t[i];
                if (tick.showLabel && (!tick.isMinorTick || this.showMinorTicks)) {
                    if (this.name == 'xaxis' || this.name == 'x2axis') {
                        temp = tick._elem.outerHeight(true);
                    }
                    else {
                        temp = tick._elem.outerWidth(true);
                    }
                    if (temp > dim) {
                        dim = temp;
                    }
                }
            }
            
            if (lshow) {
                w = this._label._elem.outerWidth(true);
                h = this._label._elem.outerHeight(true); 
            }
            if (this.name == 'xaxis') {
                dim = dim + h;
                this._elem.css({'height':dim+'px', left:'0px', bottom:'0px'});
            }
            else if (this.name == 'x2axis') {
                dim = dim + h;
                this._elem.css({'height':dim+'px', left:'0px', top:'0px'});
            }
            else if (this.name == 'yaxis') {
                dim = dim + w;
                this._elem.css({'width':dim+'px', left:'0px', top:'0px'});
                if (lshow && this._label.constructor == $.jqplot.AxisLabelRenderer) {
                    this._label._elem.css('width', w+'px');
                }
            }
            else {
                dim = dim + w;
                this._elem.css({'width':dim+'px', right:'0px', top:'0px'});
                if (lshow && this._label.constructor == $.jqplot.AxisLabelRenderer) {
                    this._label._elem.css('width', w+'px');
                }
            }
        }  
    };    
    
    // called with scope of axis
    $.jqplot.LinearAxisRenderer.prototype.createTicks = function() {
        // we're are operating on an axis here
        var ticks = this._ticks;
        var userTicks = this.ticks;
        var name = this.name;
        // databounds were set on axis initialization.
        var db = this._dataBounds;
        var dim, interval;
        var min, max;
        var pos1, pos2;
        var tt, i;
        // get a copy of user's settings for min/max.
        var userMin = this.min;
        var userMax = this.max;
        var userNT = this.numberTicks;
        var userTI = this.tickInterval;
        
        // if we already have ticks, use them.
        // ticks must be in order of increasing value.
        
        if (userTicks.length) {
            // ticks could be 1D or 2D array of [val, val, ,,,] or [[val, label], [val, label], ...] or mixed
            for (i=0; i<userTicks.length; i++){
                var ut = userTicks[i];
                var t = new this.tickRenderer(this.tickOptions);
                if (ut.constructor == Array) {
                    t.value = ut[0];
                    t.label = ut[1];
                    t.setTick(ut[0], this.name);
                    this._ticks.push(t);
                }
                
                else {
                    t.value = ut;
                    t.setTick(ut, this.name);
                    this._ticks.push(t);
                }
            }
            this.numberTicks = userTicks.length;
            this.min = this._ticks[0].value;
            this.max = this._ticks[this.numberTicks-1].value;
            this.tickInterval = (this.max - this.min) / (this.numberTicks - 1);
        }
        
        // we don't have any ticks yet, let's make some!
        else {
            if (name == 'xaxis' || name == 'x2axis') {
                dim = this._plotDimensions.width;
            }
            else {
                dim = this._plotDimensions.height;
            }
            
            // if min, max and number of ticks specified, user can't specify interval.
            if (!this.autoscale && this.min != null && this.max != null && this.numberTicks != null) {
                this.tickInterval = null;
            }
            
            // if max, min, and interval specified and interval won't fit, ignore interval.
            // if (this.min != null && this.max != null && this.tickInterval != null) {
            //     if (parseInt((this.max-this.min)/this.tickInterval, 10) != (this.max-this.min)/this.tickInterval) {
            //         this.tickInterval = null;
            //     }
            // }
        
            min = ((this.min != null) ? this.min : db.min);
            max = ((this.max != null) ? this.max : db.max);
            
            // if min and max are same, space them out a bit
            if (min == max) {
                var adj = 0.05;
                if (min > 0) {
                    adj = Math.max(Math.log(min)/Math.LN10, 0.05);
                }
                min -= adj;
                max += adj;
            }

            var range = max - min;
            var rmin, rmax;
            var temp;
            
            // autoscale.  Can't autoscale if min or max is supplied.
            // Will use numberTicks and tickInterval if supplied.  Ticks
            // across multiple axes may not line up depending on how
            // bars are to be plotted.
            if (this.autoscale && this.min == null && this.max == null) {
                var rrange, ti, margin;
                var forceMinZero = false;
                var forceZeroLine = false;
                var intervals = {min:null, max:null, average:null, stddev:null};
                // if any series are bars, or if any are fill to zero, and if this
                // is the axis to fill toward, check to see if we can start axis at zero.
                for (var i=0; i<this._series.length; i++) {
                    var s = this._series[i];
                    var faname = (s.fillAxis == 'x') ? s._xaxis.name : s._yaxis.name;
                    // check to see if this is the fill axis
                    if (this.name == faname) {
                        var vals = s._plotValues[s.fillAxis];
                        var vmin = vals[0];
                        var vmax = vals[0];
                        for (var j=1; j<vals.length; j++) {
                            if (vals[j] < vmin) {
                                vmin = vals[j];
                            }
                            else if (vals[j] > vmax) {
                                vmax = vals[j];
                            }
                        }
                        var dp = (vmax - vmin) / vmax;
                        // is this sries a bar?
                        if (s.renderer.constructor == $.jqplot.BarRenderer) {
                            // if no negative values and could also check range.
                            if (vmin >= 0 && (s.fillToZero || dp > 0.1)) {
                                forceMinZero = true;
                            }
                            else {
                                forceMinZero = false;
                                if (s.fill && s.fillToZero && vmin < 0 && vmax > 0) {
                                    forceZeroLine = true;
                                }
                                else {
                                    forceZeroLine = false;
                                }
                            }
                        }
                        
                        // if not a bar and filling, use appropriate method.
                        else if (s.fill) {
                            if (vmin >= 0 && (s.fillToZero || dp > 0.1)) {
                                forceMinZero = true;
                            }
                            else if (vmin < 0 && vmax > 0 && s.fillToZero) {
                                forceMinZero = false;
                                forceZeroLine = true;
                            }
                            else {
                                forceMinZero = false;
                                forceZeroLine = false;
                            }
                        }
                        
                        // if not a bar and not filling, only change existing state
                        // if it doesn't make sense
                        else if (vmin < 0) {
                            forceMinZero = false;
                        }
                    }
                }
                
                // check if we need make axis min at 0.
                if (forceMinZero) {
                    // compute number of ticks
                    this.numberTicks = 2 + Math.ceil((dim-(this.tickSpacing-1))/this.tickSpacing);
                    this.min = 0;
                    userMin = 0;
                    // what order is this range?
                    // what tick interval does that give us?
                    ti = max/(this.numberTicks-1);
                    temp = Math.pow(10, Math.abs(Math.floor(Math.log(ti)/Math.LN10)));
                    if (ti/temp == parseInt(ti/temp, 10)) {
                        ti += temp;
                    }
                    this.tickInterval = Math.ceil(ti/temp) * temp;
                    this.max = this.tickInterval * (this.numberTicks - 1);
                }
                
                // check if we need to make sure there is a tick at 0.
                else if (forceZeroLine) {
                    // compute number of ticks
                    this.numberTicks = 2 + Math.ceil((dim-(this.tickSpacing-1))/this.tickSpacing);
                    var ntmin = Math.ceil(Math.abs(min)/range*(this.numberTicks-1));
                    var ntmax = this.numberTicks - 1  - ntmin;
                    ti = Math.max(Math.abs(min/ntmin), Math.abs(max/ntmax));
                    temp = Math.pow(10, Math.abs(Math.floor(Math.log(ti)/Math.LN10)));
                    this.tickInterval = Math.ceil(ti/temp) * temp;
                    this.max = this.tickInterval * ntmax;
                    this.min = -this.tickInterval * ntmin;                  
                }
                
                // if nothing else, do autoscaling which will try to line up ticks across axes.
                else {  
                    if (this.numberTicks == null){
                        if (this.tickInterval) {
                            this.numberTicks = 3 + Math.ceil(range / this.tickInterval);
                        }
                        else {
                            this.numberTicks = 2 + Math.ceil((dim-(this.tickSpacing-1))/this.tickSpacing);
                        }
                    }
            
                    if (this.tickInterval == null) {
                        // get a tick interval
                        ti = range/(this.numberTicks - 1);

                        if (ti < 1) {
                            temp = Math.pow(10, Math.abs(Math.floor(Math.log(ti)/Math.LN10)));
                        }
                        else {
                            temp = 1;
                        }
                        this.tickInterval = Math.ceil(ti*temp*this.pad)/temp;
                    }
                    else {
                        temp = 1 / this.tickInterval;
                    }
                    
                    // try to compute a nicer, more even tick interval
                    // temp = Math.pow(10, Math.floor(Math.log(ti)/Math.LN10));
                    // this.tickInterval = Math.ceil(ti/temp) * temp;
                    rrange = this.tickInterval * (this.numberTicks - 1);
                    margin = (rrange - range)/2;
       
                    if (this.min == null) {
                        this.min = Math.floor(temp*(min-margin))/temp;
                    }
                    if (this.max == null) {
                        this.max = this.min + rrange;
                    }
                }
            }
            
            // Use the default algorithm which pads each axis to make the chart
            // centered nicely on the grid.
            else {
                rmin = (this.min != null) ? this.min : min - range*(this.padMin - 1);
                rmax = (this.max != null) ? this.max : max + range*(this.padMax - 1);
                this.min = rmin;
                this.max = rmax;
                range = this.max - this.min;
    
                if (this.numberTicks == null){
                    // if tickInterval is specified by user, we will ignore computed maximum.
                    // max will be equal or greater to fit even # of ticks.
                    if (this.tickInterval != null) {
                        this.numberTicks = Math.ceil((this.max - this.min)/this.tickInterval)+1;
                        this.max = this.min + this.tickInterval*(this.numberTicks-1);
                    }
                    else if (dim > 100) {
                        this.numberTicks = parseInt(3+(dim-100)/75, 10);
                    }
                    else {
                        this.numberTicks = 2;
                    }
                }
            
                if (this.tickInterval == null) {
                    this.tickInterval = range / (this.numberTicks-1);
                }
            }
            
            if (this.renderer.constructor == $.jqplot.LinearAxisRenderer) {
                // fix for misleading tick display with small range and low precision.
                range = this.max - this.min;
                // figure out precision
                var temptick = new this.tickRenderer(this.tickOptions);
                // use the tick formatString or, the default.
                var fs = temptick.formatString || $.jqplot.config.defaultTickFormatString; 
                var fs = fs.match($.jqplot.sprintf.regex)[0];
                var precision = 0;
                if (fs) {
                    if (fs.search(/[fFeEgGpP]/) > -1) {
                        var m = fs.match(/\%\.(\d{0,})?[eEfFgGpP]/);
                        if (m) precision = parseInt(m[1], 10);
                        else precision = 6;
                    }
                    else if (fs.search(/[di]/) > -1) {
                        precision = 0;
                    }
                    // fact will be <= 1;
                    var fact = Math.pow(10, -precision);
                    if (this.tickInterval < fact) {
                        // need to correct underrange
                        if (userNT == null && userTI == null) {
                            this.tickInterval = fact;
                            if (userMax == null && userMin == null) {
                                // this.min = Math.floor((this._dataBounds.min - this.tickInterval)/fact) * fact;
                                this.min = Math.floor(this._dataBounds.min/fact) * fact;
                                if (this.min == this._dataBounds.min) {
                                    this.min = this._dataBounds.min - this.tickInterval;
                                }
                                // this.max = Math.ceil((this._dataBounds.max + this.tickInterval)/fact) * fact;
                                this.max = Math.ceil(this._dataBounds.max/fact) * fact;
                                if (this.max == this._dataBounds.max) {
                                    this.max = this._dataBounds.max + this.tickInterval;
                                }
                                var n = (this.max - this.min)/this.tickInterval;
                                n = n.toFixed(11);
                                n = Math.ceil(n);
                                this.numberTicks = n + 1;
                            }
                            else if (userMax == null) {
                                // add one tick for top of range.
                                var n = (this._dataBounds.max - this.min) / this.tickInterval;
                                n = n.toFixed(11);
                                this.numberTicks = Math.ceil(n) + 2;
                                this.max = this.min + this.tickInterval * (this.numberTicks-1);
                            }
                            else if (userMin == null) {
                                // add one tick for bottom of range.
                                var n = (this.max - this._dataBounds.min) / this.tickInterval;
                                n = n.toFixed(11);
                                this.numberTicks = Math.ceil(n) + 2;
                                this.min = this.max - this.tickInterval * (this.numberTicks-1);
                            }
                            else {
                                // calculate a number of ticks so max is within axis scale
                                this.numberTicks = Math.ceil((userMax - userMin)/this.tickInterval) + 1;
                                // if user's min and max don't fit evenly in ticks, adjust.
                                // This takes care of cases such as user min set to 0, max set to 3.5 but tick
                                // format string set to %d (integer ticks)
                                this.min =  Math.floor(userMin*Math.pow(10, precision))/Math.pow(10, precision);
                                this.max =  Math.ceil(userMax*Math.pow(10, precision))/Math.pow(10, precision);
                                // this.max = this.min + this.tickInterval*(this.numberTicks-1);
                                this.numberTicks = Math.ceil((this.max - this.min)/this.tickInterval) + 1;
                            }
                        }
                    }
                }
            }
            
            

            for (var i=0; i<this.numberTicks; i++){
                tt = this.min + i * this.tickInterval;
                var t = new this.tickRenderer(this.tickOptions);
                // var t = new $.jqplot.AxisTickRenderer(this.tickOptions);

                t.setTick(tt, this.name);
                this._ticks.push(t);
            }
        }
    };
    
    // called with scope of axis
    $.jqplot.LinearAxisRenderer.prototype.pack = function(pos, offsets) {
        var ticks = this._ticks;
        var max = this.max;
        var min = this.min;
        var offmax = offsets.max;
        var offmin = offsets.min;
        var lshow = (this._label == null) ? false : this._label.show;
        
        for (var p in pos) {
            this._elem.css(p, pos[p]);
        }
        
        this._offsets = offsets;
        // pixellength will be + for x axes and - for y axes becasue pixels always measured from top left.
        var pixellength = offmax - offmin;
        var unitlength = max - min;
        
        // point to unit and unit to point conversions references to Plot DOM element top left corner.
        this.p2u = function(p){
            return (p - offmin) * unitlength / pixellength + min;
        };
        
        this.u2p = function(u){
            return (u - min) * pixellength / unitlength + offmin;
        };
                
        if (this.name == 'xaxis' || this.name == 'x2axis'){
            this.series_u2p = function(u){
                return (u - min) * pixellength / unitlength;
            };
            this.series_p2u = function(p){
                return p * unitlength / pixellength + min;
            };
        }
        
        else {
            this.series_u2p = function(u){
                return (u - max) * pixellength / unitlength;
            };
            this.series_p2u = function(p){
                return p * unitlength / pixellength + max;
            };
        }
        
        if (this.show) {
            if (this.name == 'xaxis' || this.name == 'x2axis') {
                for (i=0; i<ticks.length; i++) {
                    var t = ticks[i];
                    if (t.show && t.showLabel) {
                        var shim;
                        
                        if (t.constructor == $.jqplot.CanvasAxisTickRenderer && t.angle) {
                            // will need to adjust auto positioning based on which axis this is.
                            var temp = (this.name == 'xaxis') ? 1 : -1;
                            switch (t.labelPosition) {
                                case 'auto':
                                    // position at end
                                    if (temp * t.angle < 0) {
                                        shim = -t.getWidth() + t._textRenderer.height * Math.sin(-t._textRenderer.angle) / 2;
                                    }
                                    // position at start
                                    else {
                                        shim = -t._textRenderer.height * Math.sin(t._textRenderer.angle) / 2;
                                    }
                                    break;
                                case 'end':
                                    shim = -t.getWidth() + t._textRenderer.height * Math.sin(-t._textRenderer.angle) / 2;
                                    break;
                                case 'start':
                                    shim = -t._textRenderer.height * Math.sin(t._textRenderer.angle) / 2;
                                    break;
                                case 'middle':
                                    shim = -t.getWidth()/2 + t._textRenderer.height * Math.sin(-t._textRenderer.angle) / 2;
                                    break;
                                default:
                                    shim = -t.getWidth()/2 + t._textRenderer.height * Math.sin(-t._textRenderer.angle) / 2;
                                    break;
                            }
                        }
                        else {
                            shim = -t.getWidth()/2;
                        }
                        var val = this.u2p(t.value) + shim + 'px';
                        t._elem.css('left', val);
                        t.pack();
                    }
                }
                if (lshow) {
                    var w = this._label._elem.outerWidth(true);
                    this._label._elem.css('left', offmin + pixellength/2 - w/2 + 'px');
                    if (this.name == 'xaxis') {
                        this._label._elem.css('bottom', '0px');
                    }
                    else {
                        this._label._elem.css('top', '0px');
                    }
                    this._label.pack();
                }
            }
            else {
                for (i=0; i<ticks.length; i++) {
                    var t = ticks[i];
                    if (t.show && t.showLabel) {                        
                        var shim;
                        if (t.constructor == $.jqplot.CanvasAxisTickRenderer && t.angle) {
                            // will need to adjust auto positioning based on which axis this is.
                            var temp = (this.name == 'yaxis') ? 1 : -1;
                            switch (t.labelPosition) {
                                case 'auto':
                                    // position at end
                                case 'end':
                                    if (temp * t.angle < 0) {
                                        shim = -t._textRenderer.height * Math.cos(-t._textRenderer.angle) / 2;
                                    }
                                    else {
                                        shim = -t.getHeight() + t._textRenderer.height * Math.cos(t._textRenderer.angle) / 2;
                                    }
                                    break;
                                case 'start':
                                    if (t.angle > 0) {
                                        shim = -t._textRenderer.height * Math.cos(-t._textRenderer.angle) / 2;
                                    }
                                    else {
                                        shim = -t.getHeight() + t._textRenderer.height * Math.cos(t._textRenderer.angle) / 2;
                                    }
                                    break;
                                case 'middle':
                                    // if (t.angle > 0) {
                                    //     shim = -t.getHeight()/2 + t._textRenderer.height * Math.sin(-t._textRenderer.angle) / 2;
                                    // }
                                    // else {
                                    //     shim = -t.getHeight()/2 - t._textRenderer.height * Math.sin(t._textRenderer.angle) / 2;
                                    // }
                                    shim = -t.getHeight()/2;
                                    break;
                                default:
                                    shim = -t.getHeight()/2;
                                    break;
                            }
                        }
                        else {
                            shim = -t.getHeight()/2;
                        }
                        
                        var val = this.u2p(t.value) + shim + 'px';
                        t._elem.css('top', val);
                        t.pack();
                    }
                }
                if (lshow) {
                    var h = this._label._elem.outerHeight(true);
                    this._label._elem.css('top', offmax - pixellength/2 - h/2 + 'px');
                    if (this.name == 'yaxis') {
                        this._label._elem.css('left', '0px');
                    }
                    else {
                        this._label._elem.css('right', '0px');
                    }   
                    this._label.pack();
                }
            }
        }
    };


    // class: $.jqplot.MarkerRenderer
    // The default jqPlot marker renderer, rendering the points on the line.
    $.jqplot.MarkerRenderer = function(options){
        // Group: Properties
        
        // prop: show
        // wether or not to show the marker.
        this.show = true;
        // prop: style
        // One of diamond, circle, square, x, plus, dash, filledDiamond, filledCircle, filledSquare
        this.style = 'filledCircle';
        // prop: lineWidth
        // size of the line for non-filled markers.
        this.lineWidth = 2;
        // prop: size
        // Size of the marker (diameter or circle, length of edge of square, etc.)
        this.size = 9.0;
        // prop: color
        // color of marker.  Will be set to color of series by default on init.
        this.color = '#666666';
        // prop: shadow
        // wether or not to draw a shadow on the line
        this.shadow = true;
        // prop: shadowAngle
        // Shadow angle in degrees
        this.shadowAngle = 45;
        // prop: shadowOffset
        // Shadow offset from line in pixels
        this.shadowOffset = 1;
        // prop: shadowDepth
        // Number of times shadow is stroked, each stroke offset shadowOffset from the last.
        this.shadowDepth = 3;
        // prop: shadowAlpha
        // Alpha channel transparency of shadow.  0 = transparent.
        this.shadowAlpha = '0.07';
        // prop: shadowRenderer
        // Renderer that will draws the shadows on the marker.
        this.shadowRenderer = new $.jqplot.ShadowRenderer();
        // prop: shapeRenderer
        // Renderer that will draw the marker.
        this.shapeRenderer = new $.jqplot.ShapeRenderer();
        
        $.extend(true, this, options);
    };
    
    $.jqplot.MarkerRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
        var sdopt = {angle:this.shadowAngle, offset:this.shadowOffset, alpha:this.shadowAlpha, lineWidth:this.lineWidth, depth:this.shadowDepth, closePath:true};
        if (this.style.indexOf('filled') != -1) {
            sdopt.fill = true;
        }
        if (this.style.indexOf('ircle') != -1) {
            sdopt.isarc = true;
            sdopt.closePath = false;
        }
        this.shadowRenderer.init(sdopt);
        
        var shopt = {fill:false, isarc:false, strokeStyle:this.color, fillStyle:this.color, lineWidth:this.lineWidth, closePath:true};
        if (this.style.indexOf('filled') != -1) {
            shopt.fill = true;
        }
        if (this.style.indexOf('ircle') != -1) {
            shopt.isarc = true;
            shopt.closePath = false;
        }
        this.shapeRenderer.init(shopt);
    };
    
    $.jqplot.MarkerRenderer.prototype.drawDiamond = function(x, y, ctx, fill, options) {
        var stretch = 1.2;
        var dx = this.size/2/stretch;
        var dy = this.size/2*stretch;
        var points = [[x-dx, y], [x, y+dy], [x+dx, y], [x, y-dy]];
        if (this.shadow) {
            this.shadowRenderer.draw(ctx, points);
        }
        this.shapeRenderer.draw(ctx, points, options);

        // ctx.restore();
    };
    
    $.jqplot.MarkerRenderer.prototype.drawPlus = function(x, y, ctx, fill, options) {
        var stretch = 1.0;
        var dx = this.size/2*stretch;
        var dy = this.size/2*stretch;
        var points1 = [[x, y-dy], [x, y+dy]];
        var points2 = [[x+dx, y], [x-dx, y]];
        var opts = $.extend(true, {}, this.options, {closePath:false});
        if (this.shadow) {
            this.shadowRenderer.draw(ctx, points1, {closePath:false});
            this.shadowRenderer.draw(ctx, points2, {closePath:false});
        }
        this.shapeRenderer.draw(ctx, points1, opts);
        this.shapeRenderer.draw(ctx, points2, opts);

        // ctx.restore();
    };
    
    $.jqplot.MarkerRenderer.prototype.drawX = function(x, y, ctx, fill, options) {
        var stretch = 1.0;
        var dx = this.size/2*stretch;
        var dy = this.size/2*stretch;
        var opts = $.extend(true, {}, this.options, {closePath:false});
        var points1 = [[x-dx, y-dy], [x+dx, y+dy]];
        var points2 = [[x-dx, y+dy], [x+dx, y-dy]];
        if (this.shadow) {
            this.shadowRenderer.draw(ctx, points1, {closePath:false});
            this.shadowRenderer.draw(ctx, points2, {closePath:false});
        }
        this.shapeRenderer.draw(ctx, points1, opts);
        this.shapeRenderer.draw(ctx, points2, opts);

        // ctx.restore();
    };
    
    $.jqplot.MarkerRenderer.prototype.drawDash = function(x, y, ctx, fill, options) {
        var stretch = 1.0;
        var dx = this.size/2*stretch;
        var dy = this.size/2*stretch;
        var points = [[x-dx, y], [x+dx, y]];
        if (this.shadow) {
            this.shadowRenderer.draw(ctx, points);
        }
        this.shapeRenderer.draw(ctx, points, options);

        // ctx.restore();
    };
    
    $.jqplot.MarkerRenderer.prototype.drawSquare = function(x, y, ctx, fill, options) {
        var stretch = 1.0;
        var dx = this.size/2/stretch;
        var dy = this.size/2*stretch;
        var points = [[x-dx, y-dy], [x-dx, y+dy], [x+dx, y+dy], [x+dx, y-dy]];
        if (this.shadow) {
            this.shadowRenderer.draw(ctx, points);
        }
        this.shapeRenderer.draw(ctx, points, options);

        // ctx.restore();
    };
    
    $.jqplot.MarkerRenderer.prototype.drawCircle = function(x, y, ctx, fill, options) {
        var radius = this.size/2;
        var end = 2*Math.PI;
        var points = [x, y, radius, 0, end, true];
        if (this.shadow) {
            this.shadowRenderer.draw(ctx, points);
        }
        this.shapeRenderer.draw(ctx, points, options);
        
        // ctx.restore();
    };
    
    $.jqplot.MarkerRenderer.prototype.draw = function(x, y, ctx, options) {
        options = options || {};
        // hack here b/c shape renderer uses canvas based color style options
        // and marker uses css style names.
        if (options.show == null || options.show != false) {
            if (options.color && !options.fillStyle) {
                options.fillStyle = options.color;
            }
            if (options.color && !options.strokeStyle) {
                options.strokeStyle = options.color;
            }
            switch (this.style) {
                case 'diamond':
                    this.drawDiamond(x,y,ctx, false, options);
                    break;
                case 'filledDiamond':
                    this.drawDiamond(x,y,ctx, true, options);
                    break;
                case 'circle':
                    this.drawCircle(x,y,ctx, false, options);
                    break;
                case 'filledCircle':
                    this.drawCircle(x,y,ctx, true, options);
                    break;
                case 'square':
                    this.drawSquare(x,y,ctx, false, options);
                    break;
                case 'filledSquare':
                    this.drawSquare(x,y,ctx, true, options);
                    break;
                case 'x':
                    this.drawX(x,y,ctx, true, options);
                    break;
                case 'plus':
                    this.drawPlus(x,y,ctx, true, options);
                    break;
                case 'dash':
                    this.drawDash(x,y,ctx, true, options);
                    break;
                default:
                    this.drawDiamond(x,y,ctx, false, options);
                    break;
            }
        }
    };
    
    // class: $.jqplot.shadowRenderer
    // The default jqPlot shadow renderer, rendering shadows behind shapes.
    $.jqplot.ShadowRenderer = function(options){ 
        // Group: Properties
        
        // prop: angle
        // Angle of the shadow in degrees.  Measured counter-clockwise from the x axis.
        this.angle = 45;
        // prop: offset
        // Pixel offset at the given shadow angle of each shadow stroke from the last stroke.
        this.offset = 1;
        // prop: alpha
        // alpha transparency of shadow stroke.
        this.alpha = 0.07;
        // prop: lineWidth
        // width of the shadow line stroke.
        this.lineWidth = 1.5;
        // prop: lineJoin
        // How line segments of the shadow are joined.
        this.lineJoin = 'miter';
        // prop: lineCap
        // how ends of the shadow line are rendered.
        this.lineCap = 'round';
        // prop; closePath
        // whether line path segment is closed upon itself.
        this.closePath = false;
        // prop: fill
        // whether to fill the shape.
        this.fill = false;
        // prop: depth
        // how many times the shadow is stroked.  Each stroke will be offset by offset at angle degrees.
        this.depth = 3;
        this.strokeStyle = 'rgba(0,0,0,0.1)';
        // prop: isarc
        // wether the shadow is an arc or not.
        this.isarc = false;
        
        $.extend(true, this, options);
    };
    
    $.jqplot.ShadowRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
    };
    
    // function: draw
    // draws an transparent black (i.e. gray) shadow.
    //
    // ctx - canvas drawing context
    // points - array of points or [x, y, radius, start angle (rad), end angle (rad)]
    $.jqplot.ShadowRenderer.prototype.draw = function(ctx, points, options) {
        ctx.save();
        var opts = (options != null) ? options : {};
        var fill = (opts.fill != null) ? opts.fill : this.fill;
        var closePath = (opts.closePath != null) ? opts.closePath : this.closePath;
        var offset = (opts.offset != null) ? opts.offset : this.offset;
        var alpha = (opts.alpha != null) ? opts.alpha : this.alpha;
        var depth = (opts.depth != null) ? opts.depth : this.depth;
        var isarc = (opts.isarc != null) ? opts.isarc : this.isarc;
        ctx.lineWidth = (opts.lineWidth != null) ? opts.lineWidth : this.lineWidth;
        ctx.lineJoin = (opts.lineJoin != null) ? opts.lineJoin : this.lineJoin;
        ctx.lineCap = (opts.lineCap != null) ? opts.lineCap : this.lineCap;
        ctx.strokeStyle = opts.strokeStyle || this.strokeStyle || 'rgba(0,0,0,'+alpha+')';
        ctx.fillStyle = opts.fillStyle || this.fillStyle || 'rgba(0,0,0,'+alpha+')';
        for (var j=0; j<depth; j++) {
            ctx.translate(Math.cos(this.angle*Math.PI/180)*offset, Math.sin(this.angle*Math.PI/180)*offset);
            ctx.beginPath();
            if (isarc) {
                ctx.arc(points[0], points[1], points[2], points[3], points[4], true);                
            }
            else {
                var move = true;
                for (var i=0; i<points.length; i++) {
                    // skip to the first non-null point and move to it.
                    if (points[i][0] != null && points[i][1] != null) {
                        if (move) {
                            ctx.moveTo(points[i][0], points[i][1]);
                            move = false;
                        }
                        else {
                            ctx.lineTo(points[i][0], points[i][1]);
                        }
                    }
                    else {
                        move = true;
                    }
                }
                
            }
            if (closePath) {
                ctx.closePath();
            }
            if (fill) {
                ctx.fill();
            }
            else {
                ctx.stroke();
            }
        }
        ctx.restore();
    };
    
    // class: $.jqplot.shapeRenderer
    // The default jqPlot shape renderer.  Given a set of points will
    // plot them and either stroke a line (fill = false) or fill them (fill = true).
    // If a filled shape is desired, closePath = true must also be set to close
    // the shape.
    $.jqplot.ShapeRenderer = function(options){
        
        this.lineWidth = 1.5;
        // prop: lineJoin
        // How line segments of the shadow are joined.
        this.lineJoin = 'miter';
        // prop: lineCap
        // how ends of the shadow line are rendered.
        this.lineCap = 'round';
        // prop; closePath
        // whether line path segment is closed upon itself.
        this.closePath = false;
        // prop: fill
        // whether to fill the shape.
        this.fill = false;
        // prop: isarc
        // wether the shadow is an arc or not.
        this.isarc = false;
        // prop: fillRect
        // true to draw shape as a filled rectangle.
        this.fillRect = false;
        // prop: strokeRect
        // true to draw shape as a stroked rectangle.
        this.strokeRect = false;
        // prop: clearRect
        // true to cear a rectangle.
        this.clearRect = false;
        // prop: strokeStyle
        // css color spec for the stoke style
        this.strokeStyle = '#999999';
        // prop: fillStyle
        // css color spec for the fill style.
        this.fillStyle = '#999999'; 
        
        $.extend(true, this, options);
    };
    
    $.jqplot.ShapeRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
    };
    
    // function: draw
    // draws the shape.
    //
    // ctx - canvas drawing context
    // points - array of points for shapes or 
    // [x, y, width, height] for rectangles or
    // [x, y, radius, start angle (rad), end angle (rad)] for circles and arcs.
    $.jqplot.ShapeRenderer.prototype.draw = function(ctx, points, options) {
        ctx.save();
        var opts = (options != null) ? options : {};
        var fill = (opts.fill != null) ? opts.fill : this.fill;
        var closePath = (opts.closePath != null) ? opts.closePath : this.closePath;
        var fillRect = (opts.fillRect != null) ? opts.fillRect : this.fillRect;
        var strokeRect = (opts.strokeRect != null) ? opts.strokeRect : this.strokeRect;
        var clearRect = (opts.clearRect != null) ? opts.clearRect : this.clearRect;
        var isarc = (opts.isarc != null) ? opts.isarc : this.isarc;
        ctx.lineWidth = opts.lineWidth || this.lineWidth;
        ctx.lineJoin = opts.lineJoing || this.lineJoin;
        ctx.lineCap = opts.lineCap || this.lineCap;
        ctx.strokeStyle = (opts.strokeStyle || opts.color) || this.strokeStyle;
        ctx.fillStyle = opts.fillStyle || this.fillStyle;
        ctx.beginPath();
        if (isarc) {
            ctx.arc(points[0], points[1], points[2], points[3], points[4], true);   
            if (closePath) {
                ctx.closePath();
            }
            if (fill) {
                ctx.fill();
            }
            else {
                ctx.stroke();
            }
            ctx.restore();
            return;
        }
        else if (clearRect) {
            ctx.clearRect(points[0], points[1], points[2], points[3]);
            ctx.restore();
            return;
        }
        else if (fillRect || strokeRect) {
            if (fillRect) {
                ctx.fillRect(points[0], points[1], points[2], points[3]);
            }
            if (strokeRect) {
                ctx.strokeRect(points[0], points[1], points[2], points[3]);
                ctx.restore();
                return;
            }
        }
        else {
            var move = true;
            for (var i=0; i<points.length; i++) {
                // skip to the first non-null point and move to it.
                if (points[i][0] != null && points[i][1] != null) {
                    if (move) {
                        ctx.moveTo(points[i][0], points[i][1]);
                        move = false;
                    }
                    else {
                        ctx.lineTo(points[i][0], points[i][1]);
                    }
                }
                else {
                    move = true;
                }
            }
            if (closePath) {
                ctx.closePath();
            }
            if (fill) {
                ctx.fill();
            }
            else {
                ctx.stroke();
            }
        }
        ctx.restore();
    };
    
    // class $.jqplot.TableLegendRenderer
    // The default legend renderer for jqPlot.
    $.jqplot.TableLegendRenderer = function(){
        //
    };
    
    $.jqplot.TableLegendRenderer.prototype.init = function(options) {
        $.extend(true, this, options);
    };
        
    $.jqplot.TableLegendRenderer.prototype.addrow = function (label, color, pad, reverse) {
        var rs = (pad) ? this.rowSpacing : '0';
        if (reverse){
            var tr = $('<tr class="jqplot-table-legend"></tr>').prependTo(this._elem);
        }
        else{
            var tr = $('<tr class="jqplot-table-legend"></tr>').appendTo(this._elem);
        }
        if (this.showSwatches) {
            $('<td class="jqplot-table-legend" style="text-align:center;padding-top:'+rs+';">'+
            '<div><div class="jqplot-table-legend-swatch" style="background-color:'+color+';border-color:'+color+';"></div>'+
            '</div></td>').appendTo(tr);
        }
        if (this.showLabels) {
            var elem = $('<td class="jqplot-table-legend" style="padding-top:'+rs+';"></td>');
            elem.appendTo(tr);
            if (this.escapeHtml) {
                elem.text(label);
            }
            else {
                elem.html(label);
            }
        }
    };
    
    // called with scope of legend
    $.jqplot.TableLegendRenderer.prototype.draw = function() {
        var legend = this;
        if (this.show) {
            var series = this._series;
            // make a table.  one line label per row.
            var ss = 'position:absolute;';
            ss += (this.background) ? 'background:'+this.background+';' : '';
            ss += (this.border) ? 'border:'+this.border+';' : '';
            ss += (this.fontSize) ? 'font-size:'+this.fontSize+';' : '';
            ss += (this.fontFamily) ? 'font-family:'+this.fontFamily+';' : '';
            ss += (this.textColor) ? 'color:'+this.textColor+';' : '';
            ss += (this.marginTop != null) ? 'margin-top:'+this.marginTop+';' : '';
            ss += (this.marginBottom != null) ? 'margin-bottom:'+this.marginBottom+';' : '';
            ss += (this.marginLeft != null) ? 'margin-left:'+this.marginLeft+';' : '';
            ss += (this.marginRight != null) ? 'margin-right:'+this.marginRight+';' : '';
            this._elem = $('<table class="jqplot-table-legend" style="'+ss+'"></table>');
        
            var pad = false, 
                reverse = false;
            for (var i = 0; i< series.length; i++) {
                s = series[i];
                if (s._stack || s.renderer.constructor == $.jqplot.BezierCurveRenderer){
                    reverse = true;
                }
                if (s.show && s.showLabel) {
                    var lt = this.labels[i] || s.label.toString();
                    if (lt) {
                        var color = s.color;
                        if (reverse && i < series.length - 1){
                            pad = true;
                        }
                        else if (reverse && i == series.length - 1){
                            pad = false;
                        }
                        this.renderer.addrow.call(this, lt, color, pad, reverse);
                        pad = true;
                    }
                    // let plugins add more rows to legend.  Used by trend line plugin.
                    for (var j=0; j<$.jqplot.addLegendRowHooks.length; j++) {
                        var item = $.jqplot.addLegendRowHooks[j].call(this, s);
                        if (item) {
                            this.renderer.addrow.call(this, item.label, item.color, pad);
                            pad = true;
                        } 
                    }
                }
            }
        }
        return this._elem;
    };
    
    $.jqplot.TableLegendRenderer.prototype.pack = function(offsets) {
        if (this.show) {       
            if (this.placement == 'insideGrid') {
                switch (this.location) {
                    case 'nw':
                        var a = offsets.left;
                        var b = offsets.top;
                        this._elem.css('left', a);
                        this._elem.css('top', b);
                        break;
                    case 'n':
                        var a = (offsets.left + (this._plotDimensions.width - offsets.right))/2 - this.getWidth()/2;
                        var b = offsets.top;
                        this._elem.css('left', a);
                        this._elem.css('top', b);
                        break;
                    case 'ne':
                        var a = offsets.right;
                        var b = offsets.top;
                        this._elem.css({right:a, top:b});
                        break;
                    case 'e':
                        var a = offsets.right;
                        var b = (offsets.top + (this._plotDimensions.height - offsets.bottom))/2 - this.getHeight()/2;
                        this._elem.css({right:a, top:b});
                        break;
                    case 'se':
                        var a = offsets.right;
                        var b = offsets.bottom;
                        this._elem.css({right:a, bottom:b});
                        break;
                    case 's':
                        var a = (offsets.left + (this._plotDimensions.width - offsets.right))/2 - this.getWidth()/2;
                        var b = offsets.bottom;
                        this._elem.css({left:a, bottom:b});
                        break;
                    case 'sw':
                        var a = offsets.left;
                        var b = offsets.bottom;
                        this._elem.css({left:a, bottom:b});
                        break;
                    case 'w':
                        var a = offsets.left;
                        var b = (offsets.top + (this._plotDimensions.height - offsets.bottom))/2 - this.getHeight()/2;
                        this._elem.css({left:a, top:b});
                        break;
                    default:  // same as 'se'
                        var a = offsets.right;
                        var b = offsets.bottom;
                        this._elem.css({right:a, bottom:b});
                        break;
                }
                
            }
            else if (this.placement == 'outside'){
                switch (this.location) {
                    case 'nw':
                        var a = this._plotDimensions.width - offsets.left;
                        var b = offsets.top;
                        this._elem.css('right', a);
                        this._elem.css('top', b);
                        break;
                    case 'n':
                        var a = (offsets.left + (this._plotDimensions.width - offsets.right))/2 - this.getWidth()/2;
                        var b = this._plotDimensions.height - offsets.top;
                        this._elem.css('left', a);
                        this._elem.css('bottom', b);
                        break;
                    case 'ne':
                        var a = this._plotDimensions.width - offsets.right;
                        var b = offsets.top;
                        this._elem.css({left:a, top:b});
                        break;
                    case 'e':
                        var a = this._plotDimensions.width - offsets.right;
                        var b = (offsets.top + (this._plotDimensions.height - offsets.bottom))/2 - this.getHeight()/2;
                        this._elem.css({left:a, top:b});
                        break;
                    case 'se':
                        var a = this._plotDimensions.width - offsets.right;
                        var b = offsets.bottom;
                        this._elem.css({left:a, bottom:b});
                        break;
                    case 's':
                        var a = (offsets.left + (this._plotDimensions.width - offsets.right))/2 - this.getWidth()/2;
                        var b = this._plotDimensions.height - offsets.bottom;
                        this._elem.css({left:a, top:b});
                        break;
                    case 'sw':
                        var a = this._plotDimensions.width - offsets.left;
                        var b = offsets.bottom;
                        this._elem.css({right:a, bottom:b});
                        break;
                    case 'w':
                        var a = this._plotDimensions.width - offsets.left;
                        var b = (offsets.top + (this._plotDimensions.height - offsets.bottom))/2 - this.getHeight()/2;
                        this._elem.css({right:a, top:b});
                        break;
                    default:  // same as 'se'
                        var a = offsets.right;
                        var b = offsets.bottom;
                        this._elem.css({right:a, bottom:b});
                        break;
                }
            }
            else {
                switch (this.location) {
                    case 'nw':
                        this._elem.css({left:0, top:offsets.top});
                        break;
                    case 'n':
                        var a = (offsets.left + (this._plotDimensions.width - offsets.right))/2 - this.getWidth()/2;
                        this._elem.css({left: a, top:offsets.top});
                        break;
                    case 'ne':
                        this._elem.css({right:0, top:offsets.top});
                        break;
                    case 'e':
                        var b = (offsets.top + (this._plotDimensions.height - offsets.bottom))/2 - this.getHeight()/2;
                        this._elem.css({right:offsets.right, top:b});
                        break;
                    case 'se':
                        this._elem.css({right:offsets.right, bottom:offsets.bottom});
                        break;
                    case 's':
                        var a = (offsets.left + (this._plotDimensions.width - offsets.right))/2 - this.getWidth()/2;
                        this._elem.css({left: a, bottom:offsets.bottom});
                        break;
                    case 'sw':
                        this._elem.css({left:offsets.left, bottom:offsets.bottom});
                        break;
                    case 'w':
                        var b = (offsets.top + (this._plotDimensions.height - offsets.bottom))/2 - this.getHeight()/2;
                        this._elem.css({left:offsets.left, top:b});
                        break;
                    default:  // same as 'se'
                        this._elem.css({right:offsets.right, bottom:offsets.bottom});
                        break;
                }
            }
        } 
    };

    /**
     * Class: $.jqplot.ThemeEngine
     * Theme Engine provides a programatic way to change some of the  more
     * common jqplot styling options such as fonts, colors and grid options.
     * A theme engine instance is created with each plot.  The theme engine
     * manages a collection of themes which can be modified, added to, or 
     * applied to the plot.
     * 
     * The themeEngine class is not instantiated directly.
     * When a plot is initialized, the current plot options are scanned
     * an a default theme named "Default" is created.  This theme is
     * used as the basis for other themes added to the theme engine and
     * is always available.
     * 
     * A theme is a simple javascript object with styling parameters for
     * various entities of the plot.  A theme has the form:
     * 
     * 
     * > {
     * >     _name:f "Default",
     * >     target: {
     * >         backgroundColor: "transparent"
     * >     },
     * >     legend: {
     * >         textColor: null,
     * >         fontFamily: null,
     * >         fontSize: null,
     * >         border: null,
     * >         background: null
     * >     },
     * >     title: {
     * >         textColor: "rgb(102, 102, 102)",
     * >         fontFamily: "'Trebuchet MS',Arial,Helvetica,sans-serif",
     * >         fontSize: "19.2px",
     * >         textAlign: "center"
     * >     },
     * >     seriesStyles: {},
     * >     series: [{
     * >         color: "#4bb2c5",
     * >         lineWidth: 2.5,
     * >         shadow: true,
     * >         fillColor: "#4bb2c5",
     * >         showMarker: true,
     * >         markerOptions: {
     * >             color: "#4bb2c5",
     * >             show: true,
     * >             style: 'filledCircle',
     * >             lineWidth: 1.5,
     * >             size: 4,
     * >             shadow: true
     * >         }
     * >     }],
     * >     grid: {
     * >         drawGridlines: true,
     * >         gridLineColor: "#cccccc",
     * >         gridLineWidth: 1,
     * >         backgroundColor: "#fffdf6",
     * >         borderColor: "#999999",
     * >         borderWidth: 2,
     * >         shadow: true
     * >     },
     * >     axesStyles: {
     * >         label: {},
     * >         ticks: {}
     * >     },
     * >     axes: {
     * >         xaxis: {
     * >             borderColor: "#999999",
     * >             borderWidth: 2,
     * >             ticks: {
     * >                 show: true,
     * >                 showGridline: true,
     * >                 showLabel: true,
     * >                 showMark: true,
     * >                 size: 4,
     * >                 textColor: "",
     * >                 whiteSpace: "nowrap",
     * >                 fontSize: "12px",
     * >                 fontFamily: "'Trebuchet MS',Arial,Helvetica,sans-serif"
     * >             },
     * >             label: {
     * >                 textColor: "rgb(102, 102, 102)",
     * >                 whiteSpace: "normal",
     * >                 fontSize: "14.6667px",
     * >                 fontFamily: "'Trebuchet MS',Arial,Helvetica,sans-serif",
     * >                 fontWeight: "400"
     * >             }
     * >         },
     * >         yaxis: {
     * >             borderColor: "#999999",
     * >             borderWidth: 2,
     * >             ticks: {
     * >                 show: true,
     * >                 showGridline: true,
     * >                 showLabel: true,
     * >                 showMark: true,
     * >                 size: 4,
     * >                 textColor: "",
     * >                 whiteSpace: "nowrap",
     * >                 fontSize: "12px",
     * >                 fontFamily: "'Trebuchet MS',Arial,Helvetica,sans-serif"
     * >             },
     * >             label: {
     * >                 textColor: null,
     * >                 whiteSpace: null,
     * >                 fontSize: null,
     * >                 fontFamily: null,
     * >                 fontWeight: null
     * >             }
     * >         },
     * >         x2axis: {...
     * >         },
     * >         ...
     * >         y9axis: {...
     * >         }
     * >     }
     * > }
     * 
     * "seriesStyles" is a style object that will be applied to all series in the plot.
     * It will forcibly override any styles applied on the individual series.  "axesStyles" is
     * a style object that will be applied to all axes in the plot.  It will also forcibly
     * override any styles on the individual axes.
     * 
     * The example shown above has series options for a line series.  Options for other
     * series types are shown below:
     * 
     * Bar Series:
     * 
     * > {
     * >     color: "#4bb2c5",
     * >     seriesColors: ["#4bb2c5", "#EAA228", "#c5b47f", "#579575", "#839557", "#958c12", "#953579", "#4b5de4", "#d8b83f", "#ff5800", "#0085cc", "#c747a3", "#cddf54", "#FBD178", "#26B4E3", "#bd70c7"],
     * >     lineWidth: 2.5,
     * >     shadow: true,
     * >     barPadding: 2,
     * >     barMargin: 10,
     * >     barWidth: 15.09375,
     * >     highlightColors: ["rgb(129,201,214)", "rgb(129,201,214)", "rgb(129,201,214)", "rgb(129,201,214)", "rgb(129,201,214)", "rgb(129,201,214)", "rgb(129,201,214)", "rgb(129,201,214)"]
     * > }
     * 
     * Pie Series:
     * 
     * > {
     * >     seriesColors: ["#4bb2c5", "#EAA228", "#c5b47f", "#579575", "#839557", "#958c12", "#953579", "#4b5de4", "#d8b83f", "#ff5800", "#0085cc", "#c747a3", "#cddf54", "#FBD178", "#26B4E3", "#bd70c7"],
     * >     padding: 20,
     * >     sliceMargin: 0,
     * >     fill: true,
     * >     shadow: true,
     * >     startAngle: 0,
     * >     lineWidth: 2.5,
     * >     highlightColors: ["rgb(129,201,214)", "rgb(240,189,104)", "rgb(214,202,165)", "rgb(137,180,158)", "rgb(168,180,137)", "rgb(180,174,89)", "rgb(180,113,161)", "rgb(129,141,236)", "rgb(227,205,120)", "rgb(255,138,76)", "rgb(76,169,219)", "rgb(215,126,190)", "rgb(220,232,135)", "rgb(200,167,96)", "rgb(103,202,235)", "rgb(208,154,215)"]
     * > }
     * 
     * Funnel Series:
     * 
     * > {
     * >     color: "#4bb2c5",
     * >     lineWidth: 2,
     * >     shadow: true,
     * >     padding: {
     * >         top: 20,
     * >         right: 20,
     * >         bottom: 20,
     * >         left: 20
     * >     },
     * >     sectionMargin: 6,
     * >     seriesColors: ["#4bb2c5", "#EAA228", "#c5b47f", "#579575", "#839557", "#958c12", "#953579", "#4b5de4", "#d8b83f", "#ff5800", "#0085cc", "#c747a3", "#cddf54", "#FBD178", "#26B4E3", "#bd70c7"],
     * >     highlightColors: ["rgb(147,208,220)", "rgb(242,199,126)", "rgb(220,210,178)", "rgb(154,191,172)", "rgb(180,191,154)", "rgb(191,186,112)", "rgb(191,133,174)", "rgb(147,157,238)", "rgb(231,212,139)", "rgb(255,154,102)", "rgb(102,181,224)", "rgb(221,144,199)", "rgb(225,235,152)", "rgb(200,167,96)", "rgb(124,210,238)", "rgb(215,169,221)"]
     * > }
     * 
     */
    $.jqplot.ThemeEngine = function(){
        // Group: Properties
        //
        // prop: themes
        // hash of themes managed by the theme engine.  
        // Indexed by theme name.
        this.themes = {};
        // prop: activeTheme
        // Pointer to currently active theme
        this.activeTheme=null;
        
    };
    
    // called with scope of plot
    $.jqplot.ThemeEngine.prototype.init = function() {
        // get the Default theme from the current plot settings.
        var th = new $.jqplot.Theme({_name:'Default'});
        var n, i;
        
        for (n in th.target) {
            if (n == "textColor") {
                th.target[n] = this.target.css('color');
            }
            else {
                th.target[n] = this.target.css(n);
            }
        }
        
        if (this.title.show && this.title._elem) {
            for (n in th.title) {
                if (n == "textColor") {
                    th.title[n] = this.title._elem.css('color');
                }
                else {
                    th.title[n] = this.title._elem.css(n);
                }
            }
        }
        
        for (n in th.grid) {
            th.grid[n] = this.grid[n];
        }
        if (th.grid.backgroundColor == null && this.grid.background != null) {
            th.grid.backgroundColor = this.grid.background;
        }
        if (this.legend.show && this.legend._elem) {
            for (n in th.legend) {
                if (n == 'textColor') {
                    th.legend[n] = this.legend._elem.css('color');
                }
                else {
                    th.legend[n] = this.legend._elem.css(n);
                }
            }
        }
        var s;
        
        for (i=0; i<this.series.length; i++) {
            s = this.series[i];
            if (s.renderer.constructor == $.jqplot.LineRenderer) {
                th.series.push(new LineSeriesProperties());
            }
            else if (s.renderer.constructor == $.jqplot.BarRenderer) {
                th.series.push(new BarSeriesProperties());
            }
            else if (s.renderer.constructor == $.jqplot.PieRenderer) {
                th.series.push(new PieSeriesProperties());
            }
            else if (s.renderer.constructor == $.jqplot.DonutRenderer) {
                th.series.push(new DonutSeriesProperties());
            }
            else if (s.renderer.constructor == $.jqplot.FunnelRenderer) {
                th.series.push(new FunnelSeriesProperties());
            }
            else if (s.renderer.constructor == $.jqplot.MeterGaugeRenderer) {
                th.series.push(new MeterSeriesProperties());
            }
            else {
                th.series.push({});
            }
            for (n in th.series[i]) {
                th.series[i][n] = s[n];
            }
        }
        var a, ax;
        for (n in this.axes) {
            ax = this.axes[n];
            a = th.axes[n] = new AxisProperties();
            a.borderColor = ax.borderColor;
            a.borderWidth = ax.borderWidth;
            if (ax._ticks && ax._ticks[0]) {
                for (nn in a.ticks) {
                    if (ax._ticks[0].hasOwnProperty(nn)) {
                        a.ticks[nn] = ax._ticks[0][nn];
                    }
                    else if (ax._ticks[0]._elem){
                        a.ticks[nn] = ax._ticks[0]._elem.css(nn);
                    }
                }
            }
            if (ax._label && ax._label.show) {
                for (nn in a.label) {
                    // a.label[nn] = ax._label._elem.css(nn);
                    if (ax._label[nn]) {
                        a.label[nn] = ax._label[nn];
                    }
                    else if (ax._label._elem){
                        if (nn == 'textColor') {
                            a.label[nn] = ax._label._elem.css('color');
                        }
                        else {
                            a.label[nn] = ax._label._elem.css(nn);
                        }
                    }
                }
            }
        }
        this.themeEngine._add(th);
        this.themeEngine.activeTheme  = this.themeEngine.themes[th._name];
    };
    /**
     * Group: methods
     * 
     * method: get
     * 
     * Get and return the named theme or the active theme if no name given.
     * 
     * parameter:
     * 
     * name - name of theme to get.
     * 
     * returns:
     * 
     * Theme instance of given name.
     */   
    $.jqplot.ThemeEngine.prototype.get = function(name) {
        if (!name) {
            // return the active theme
            return this.activeTheme;
        }
        else {
            return this.themes[name];
        }
    };
    
    function numericalOrder(a,b) { return a-b; }
    
    /**
     * method: getThemeNames
     * 
     * Return the list of theme names in this manager in alpha-numerical order.
     * 
     * parameter:
     * 
     * None
     * 
     * returns:
     * 
     * A the list of theme names in this manager in alpha-numerical order.
     */       
    $.jqplot.ThemeEngine.prototype.getThemeNames = function() {
        var tn = [];
        for (var n in this.themes) {
            tn.push(n);
        }
        return tn.sort(numericalOrder);
    };

    /**
     * method: getThemes
     * 
     * Return a list of themes in alpha-numerical order by name.
     * 
     * parameter:
     * 
     * None
     * 
     * returns:
     * 
     * A list of themes in alpha-numerical order by name.
     */ 
    $.jqplot.ThemeEngine.prototype.getThemes = function() {
        var tn = [];
        var themes = [];
        for (var n in this.themes) {
            tn.push(n);
        }
        tn.sort(numericalOrder);
        for (var i=0; i<tn.length; i++) {
            themes.push(this.themes[tn[i]]);
        }
        return themes;
    };
    
    $.jqplot.ThemeEngine.prototype.activate = function(plot, name) {
        // sometimes need to redraw whole plot.
        var redrawPlot = false;
        if (!name && this.activeTheme && this.activeTheme._name) {
            name = this.activeTheme._name;
        }
        if (!this.themes.hasOwnProperty(name)) {
            throw new Error("No theme of that name");
        }
        else {
            var th = this.themes[name];
            this.activeTheme = th;
            var val, checkBorderColor = false, checkBorderWidth = false;
            var arr = ['xaxis', 'x2axis', 'yaxis', 'y2axis'];
            
            for (i=0; i<arr.length; i++) {
                var ax = arr[i];
                if (th.axesStyles.borderColor != null) {
                    plot.axes[ax].borderColor = th.axesStyles.borderColor;
                }
                if (th.axesStyles.borderWidth != null) {
                    plot.axes[ax].borderWidth = th.axesStyles.borderWidth;
                }
            }
            
            for (axname in plot.axes) {
                var axis = plot.axes[axname];
                if (axis.show) {
                    var thaxis = th.axes[axname] || {};
                    var thaxstyle = th.axesStyles;
                    var thax = $.jqplot.extend(true, {}, thaxis, thaxstyle);
                    val = (th.axesStyles.borderColor != null) ? th.axesStyles.borderColor : thax.borderColor;
                    if (thax.borderColor != null) {
                        axis.borderColor = thax.borderColor;
                        redrawPlot = true;
                    }
                    val = (th.axesStyles.borderWidth != null) ? th.axesStyles.borderWidth : thax.borderWidth;
                    if (thax.borderWidth != null) {
                        axis.borderWidth = thax.borderWidth;
                        redrawPlot = true;
                    }
                    if (axis._ticks && axis._ticks[0]) {
                        for (nn in thax.ticks) {
                            // val = null;
                            // if (th.axesStyles.ticks && th.axesStyles.ticks[nn] != null) {
                            //     val = th.axesStyles.ticks[nn];
                            // }
                            // else if (thax.ticks[nn] != null){
                            //     val = thax.ticks[nn]
                            // }
                            val = thax.ticks[nn];
                            if (val != null) {
                                axis.tickOptions[nn] = val;
                                axis._ticks = [];
                                redrawPlot = true;
                            }
                        }
                    }
                    if (axis._label && axis._label.show) {
                        for (nn in thax.label) {
                            // val = null;
                            // if (th.axesStyles.label && th.axesStyles.label[nn] != null) {
                            //     val = th.axesStyles.label[nn];
                            // }
                            // else if (thax.label && thax.label[nn] != null){
                            //     val = thax.label[nn]
                            // }
                            val = thax.label[nn];
                            if (val != null) {
                                axis.labelOptions[nn] = val;
                                redrawPlot = true;
                            }
                        }
                    }
                    
                }
            }            
            
            for (var n in th.grid) {
                if (th.grid[n] != null) {
                    plot.grid[n] = th.grid[n];
                }
            }
            if (!redrawPlot) {
                plot.grid.draw();
            }
            
            if (plot.legend.show) { 
                for (n in th.legend) {
                    if (th.legend[n] != null) {
                        plot.legend[n] = th.legend[n];
                    }
                }
            }
            if (plot.title.show) {
                for (n in th.title) {
                    if (th.title[n] != null) {
                        plot.title[n] = th.title[n];
                    }
                }
            }
            
            var i;
            for (i=0; i<th.series.length; i++) {
                var opts = {};
                var redrawSeries = false;
                for (n in th.series[i]) {
                    val = (th.seriesStyles[n] != null) ? th.seriesStyles[n] : th.series[i][n];
                    if (val != null) {
                        opts[n] = val;
                        if (n == 'color') {
                            plot.series[i].renderer.shapeRenderer.fillStyle = val;
                            plot.series[i].renderer.shapeRenderer.strokeStyle = val;
                            plot.series[i][n] = val;
                        }
                        else if (n == 'lineWidth') {
                            plot.series[i].renderer.shapeRenderer.lineWidth = val;
                            plot.series[i][n] = val;
                        }
                        else if (n == 'markerOptions') {
                            merge (plot.series[i].markerOptions, val);
                            merge (plot.series[i].markerRenderer, val);
                        }
                        else {
                            plot.series[i][n] = val;
                        }
                        redrawPlot = true;
                    }
                }
            }
            
            if (redrawPlot) {
                plot.target.empty();
                plot.draw();
            }
            
            for (n in th.target) {
                if (th.target[n] != null) {
                    plot.target.css(n, th.target[n]);
                }
            }
        }
        
    };
    
    $.jqplot.ThemeEngine.prototype._add = function(theme, name) {
        if (name) {
            theme._name = name;
        }
        if (!theme._name) {
            theme._name = Date.parse(new Date());
        }
        if (!this.themes.hasOwnProperty(theme._name)) {
            this.themes[theme._name] = theme;
        }
        else {
            throw new Error("jqplot.ThemeEngine Error: Theme already in use");
        }
    };
    
    // method remove
    // Delete the named theme, return true on success, false on failure.
    

    /**
     * method: remove
     * 
     * Remove the given theme from the themeEngine.
     * 
     * parameters:
     * 
     * name - name of the theme to remove.
     * 
     * returns:
     * 
     * true on success, false on failure.
     */
    $.jqplot.ThemeEngine.prototype.remove = function(name) {
        if (name == 'Default') {
            return false;
        }
        return delete this.themes[name];
    };

    /**
     * method: newTheme
     * 
     * Create a new theme based on the default theme, adding it the themeEngine.
     * 
     * parameters:
     * 
     * name - name of the new theme.
     * obj - optional object of styles to be applied to this new theme.
     * 
     * returns:
     * 
     * new Theme object.
     */
    $.jqplot.ThemeEngine.prototype.newTheme = function(name, obj) {
        if (typeof(name) == 'object') {
            obj = obj || name;
            name = null;
        }
        if (obj && obj._name) {
            name = obj._name;
        }
        else {
            name = name || Date.parse(new Date());
        }
        // var th = new $.jqplot.Theme(name);
        var th = this.copy(this.themes['Default']._name, name);
        $.jqplot.extend(th, obj);
        return th;
    };
    
    // function clone(obj) {
    //     return eval(obj.toSource());
    // }
    
    function clone(obj){
        if(obj == null || typeof(obj) != 'object'){
            return obj;
        }
    
        var temp = new obj.constructor();
        for(var key in obj){
            temp[key] = clone(obj[key]);
        }   
        return temp;
    }
    
    $.jqplot.clone = clone;
    
    function merge(obj1, obj2) {
        if (obj2 ==  null || typeof(obj2) != 'object') {
            return;
        }
        for (var key in obj2) {
            if (key == 'highlightColors') {
                obj1[key] = clone(obj2[key]);
            }
            if (obj2[key] != null && typeof(obj2[key]) == 'object') {
                if (!obj1.hasOwnProperty(key)) {
                    obj1[key] = {};
                }
                merge(obj1[key], obj2[key]);
            }
            else {
                obj1[key] = obj2[key];
            }
        }
    }
    
    $.jqplot.merge = merge;
    
        // Use the jQuery 1.3.2 extend function since behaviour in jQuery 1.4 seems problematic
    $.jqplot.extend = function() {
    	// copy reference to target object
    	var target = arguments[0] || {}, i = 1, length = arguments.length, deep = false, options;

    	// Handle a deep copy situation
    	if ( typeof target === "boolean" ) {
    		deep = target;
    		target = arguments[1] || {};
    		// skip the boolean and the target
    		i = 2;
    	}

    	// Handle case when target is a string or something (possible in deep copy)
    	if ( typeof target !== "object" && !toString.call(target) === "[object Function]" ) {
    	    target = {};
    	}

    	for ( ; i < length; i++ ){
    		// Only deal with non-null/undefined values
    		if ( (options = arguments[ i ]) != null ) {
    			// Extend the base object
    			for ( var name in options ) {
    				var src = target[ name ], copy = options[ name ];

    				// Prevent never-ending loop
    				if ( target === copy ) {
    					continue;
    				}

    				// Recurse if we're merging object values
    				if ( deep && copy && typeof copy === "object" && !copy.nodeType ) {
    					target[ name ] = $.jqplot.extend( deep, 
    						// Never move original objects, clone them
    						src || ( copy.length != null ? [ ] : { } )
    					, copy );
                    }
    				// Don't bring in undefined values
    				else if ( copy !== undefined ) {
    					target[ name ] = copy;
    				}
    			}
    		}
        }
    	// Return the modified object
    	return target;
    };

    /**
     * method: rename
     * 
     * Rename a theme.
     * 
     * parameters:
     * 
     * oldName - current name of the theme.
     * newName - desired name of the theme.
     * 
     * returns:
     * 
     * new Theme object.
     */
    $.jqplot.ThemeEngine.prototype.rename = function (oldName, newName) {
        if (oldName == 'Default' || newName == 'Default') {
            throw new Error ("jqplot.ThemeEngine Error: Cannot rename from/to Default");
        }
        if (this.themes.hasOwnProperty(newName)) {
            throw new Error ("jqplot.ThemeEngine Error: New name already in use.");
        }
        else if (this.themes.hasOwnProperty(oldName)) {
            var th = this.copy (oldName, newName);
            this.remove(oldName);
            return th;
        }
        throw new Error("jqplot.ThemeEngine Error: Old name or new name invalid");
    };

    /**
     * method: copy
     * 
     * Create a copy of an existing theme in the themeEngine, adding it the themeEngine.
     * 
     * parameters:
     * 
     * sourceName - name of the existing theme.
     * targetName - name of the copy.
     * obj - optional object of style parameter to apply to the new theme.
     * 
     * returns:
     * 
     * new Theme object.
     */
    $.jqplot.ThemeEngine.prototype.copy = function (sourceName, targetName, obj) {
        if (targetName == 'Default') {
            throw new Error ("jqplot.ThemeEngine Error: Cannot copy over Default theme");
        }
        if (!this.themes.hasOwnProperty(sourceName)) {
            var s = "jqplot.ThemeEngine Error: Source name invalid";
            throw new Error(s);
        }
        if (this.themes.hasOwnProperty(targetName)) {
            var s = "jqplot.ThemeEngine Error: Target name invalid";
            throw new Error(s);
        }
        else {
            var th = clone(this.themes[sourceName]);
            th._name = targetName;
            $.jqplot.extend(true, th, obj);
            this._add(th);
            return th;
        }
    };
    
    
    $.jqplot.Theme = function(name, obj) {
        if (typeof(name) == 'object') {
            obj = obj || name;
            name = null;
        }
        name = name || Date.parse(new Date());
        this._name = name;
        this.target = {
            backgroundColor: null
        };
        this.legend = {
            textColor: null,
            fontFamily: null,
            fontSize: null,
            border: null,
            background: null
        };
        this.title = {
            textColor: null,
            fontFamily: null,
            fontSize: null,
            textAlign: null
        };
        this.seriesStyles = {};
        this.series = [];
        this.grid = {
            drawGridlines: null,
            gridLineColor: null,
            gridLineWidth: null,
            backgroundColor: null,
            borderColor: null,
            borderWidth: null,
            shadow: null
        };
        this.axesStyles = {label:{}, ticks:{}};
        this.axes = {};
        if (typeof(obj) == 'string') {
            this._name = obj;
        }
        else if(typeof(obj) == 'object') {
            $.jqplot.extend(true, this, obj);
        }
    };
    
    var AxisProperties = function() {
        this.borderColor = null;
        this.borderWidth = null;
        this.ticks = new AxisTicks();
        this.label = new AxisLabel();
    };
    
    var AxisTicks = function() {
        this.show = null;
        this.showGridline = null;
        this.showLabel = null;
        this.showMark = null;
        this.size = null;
        this.textColor = null;
        this.whiteSpace = null;
        this.fontSize = null;
        this.fontFamily = null;
    };
    
    var AxisLabel = function() {
        this.textColor = null;
        this.whiteSpace = null;
        this.fontSize = null;
        this.fontFamily = null;
        this.fontWeight = null;
    };
    
    var LineSeriesProperties = function() {
        this.color=null;
        this.lineWidth=null;
        this.shadow=null;
        this.fillColor=null;
        this.showMarker=null;
        this.markerOptions = new MarkerOptions();
    };
    
    var MarkerOptions = function() {
        this.show = null;
        this.style = null;
        this.lineWidth = null;
        this.size = null;
        this.color = null;
        this.shadow = null;
    };
    
    var BarSeriesProperties = function() {
        this.color=null;
        this.seriesColors=null;
        this.lineWidth=null;
        this.shadow=null;
        this.barPadding=null;
        this.barMargin=null;
        this.barWidth=null;
        this.highlightColors=null;
    };
    
    var PieSeriesProperties = function() {
        this.seriesColors=null;
        this.padding=null;
        this.sliceMargin=null;
        this.fill=null;
        this.shadow=null;
        this.startAngle=null;
        this.lineWidth=null;
        this.highlightColors=null;
    };
    
    var DonutSeriesProperties = function() {
        this.seriesColors=null;
        this.padding=null;
        this.sliceMargin=null;
        this.fill=null;
        this.shadow=null;
        this.startAngle=null;
        this.lineWidth=null;
        this.innerDiameter=null;
        this.thickness=null;
        this.ringMargin=null;
        this.highlightColors=null;
    };
    
    var FunnelSeriesProperties = function() {
        this.color=null;
        this.lineWidth=null;
        this.shadow=null;
        this.padding=null;
        this.sectionMargin=null;
        this.seriesColors=null;
        this.highlightColors=null;
    };
    
    var MeterSeriesProperties = function() {
        this.padding=null;
        this.backgroundColor=null;
        this.ringColor=null;
        this.tickColor=null;
        this.ringWidth=null;
        this.intervalColors=null;
        this.intervalInnerRadius=null;
        this.intervalOuterRadius=null;
        this.hubRadius=null;
        this.needleThickness=null;
        this.needlePad=null;
    };
        

      
    /**
     * JavaScript printf/sprintf functions.
     * 
     * This code has been adapted from the publicly available sprintf methods
     * by Ash Searle. His original header follows:
     *
     *     This code is unrestricted: you are free to use it however you like.
     *     
     *     The functions should work as expected, performing left or right alignment,
     *     truncating strings, outputting numbers with a required precision etc.
     *
     *     For complex cases, these functions follow the Perl implementations of
     *     (s)printf, allowing arguments to be passed out-of-order, and to set the
     *     precision or length of the output based on arguments instead of fixed
     *     numbers.
     *
     *     See http://perldoc.perl.org/functions/sprintf.html for more information.
     *
     *     Implemented:
     *     - zero and space-padding
     *     - right and left-alignment,
     *     - base X prefix (binary, octal and hex)
     *     - positive number prefix
     *     - (minimum) width
     *     - precision / truncation / maximum width
     *     - out of order arguments
     *
     *     Not implemented (yet):
     *     - vector flag
     *     - size (bytes, words, long-words etc.)
     *     
     *     Will not implement:
     *     - %n or %p (no pass-by-reference in JavaScript)
     *
     *     @version 2007.04.27
     *     @author Ash Searle 
     * 
     * You can see the original work and comments on his blog:
     * http://hexmen.com/blog/2007/03/printf-sprintf/
     * http://hexmen.com/js/sprintf.js
     */
     
     /**
      * @Modifications 2009.05.26
      * @author Chris Leonello
      * 
      * Added %p %P specifier
      * Acts like %g or %G but will not add more significant digits to the output than present in the input.
      * Example:
      * Format: '%.3p', Input: 0.012, Output: 0.012
      * Format: '%.3g', Input: 0.012, Output: 0.0120
      * Format: '%.4p', Input: 12.0, Output: 12.0
      * Format: '%.4g', Input: 12.0, Output: 12.00
      * Format: '%.4p', Input: 4.321e-5, Output: 4.321e-5
      * Format: '%.4g', Input: 4.321e-5, Output: 4.3210e-5
      */
    $.jqplot.sprintf = function() {
        function pad(str, len, chr, leftJustify) {
            var padding = (str.length >= len) ? '' : Array(1 + len - str.length >>> 0).join(chr);
            return leftJustify ? str + padding : padding + str;

        }

        function justify(value, prefix, leftJustify, minWidth, zeroPad, htmlSpace) {
            var diff = minWidth - value.length;
            if (diff > 0) {
                var spchar = ' ';
                if (htmlSpace) { spchar = '&nbsp;'; }
                if (leftJustify || !zeroPad) {
                    value = pad(value, minWidth, spchar, leftJustify);
                } else {
                    value = value.slice(0, prefix.length) + pad('', diff, '0', true) + value.slice(prefix.length);
                }
            }
            return value;
        }

        function formatBaseX(value, base, prefix, leftJustify, minWidth, precision, zeroPad, htmlSpace) {
            // Note: casts negative numbers to positive ones
            var number = value >>> 0;
            prefix = prefix && number && {'2': '0b', '8': '0', '16': '0x'}[base] || '';
            value = prefix + pad(number.toString(base), precision || 0, '0', false);
            return justify(value, prefix, leftJustify, minWidth, zeroPad, htmlSpace);
        }

        function formatString(value, leftJustify, minWidth, precision, zeroPad, htmlSpace) {
            if (precision != null) {
                value = value.slice(0, precision);
            }
            return justify(value, '', leftJustify, minWidth, zeroPad, htmlSpace);
        }

        var a = arguments, i = 0, format = a[i++];

        return format.replace($.jqplot.sprintf.regex, function(substring, valueIndex, flags, minWidth, _, precision, type) {
            if (substring == '%%') { return '%'; }

            // parse flags
            var leftJustify = false, positivePrefix = '', zeroPad = false, prefixBaseX = false, htmlSpace = false;
                for (var j = 0; flags && j < flags.length; j++) switch (flags.charAt(j)) {
                case ' ': positivePrefix = ' '; break;
                case '+': positivePrefix = '+'; break;
                case '-': leftJustify = true; break;
                case '0': zeroPad = true; break;
                case '#': prefixBaseX = true; break;
                case '&': htmlSpace = true; break;
            }

            // parameters may be null, undefined, empty-string or real valued
            // we want to ignore null, undefined and empty-string values

            if (!minWidth) {
                minWidth = 0;
            } 
            else if (minWidth == '*') {
                minWidth = +a[i++];
            } 
            else if (minWidth.charAt(0) == '*') {
                minWidth = +a[minWidth.slice(1, -1)];
            } 
            else {
                minWidth = +minWidth;
            }

            // Note: undocumented perl feature:
            if (minWidth < 0) {
                minWidth = -minWidth;
                leftJustify = true;
            }

            if (!isFinite(minWidth)) {
                throw new Error('$.jqplot.sprintf: (minimum-)width must be finite');
            }

            if (!precision) {
                precision = 'fFeE'.indexOf(type) > -1 ? 6 : (type == 'd') ? 0 : void(0);
            } 
            else if (precision == '*') {
                precision = +a[i++];
            } 
            else if (precision.charAt(0) == '*') {
                precision = +a[precision.slice(1, -1)];
            } 
            else {
                precision = +precision;
            }

            // grab value using valueIndex if required?
            var value = valueIndex ? a[valueIndex.slice(0, -1)] : a[i++];

            switch (type) {
            case 's': {
                if (value == null) {
                    return '';
                }
                return formatString(String(value), leftJustify, minWidth, precision, zeroPad, htmlSpace);
            }
            case 'c': return formatString(String.fromCharCode(+value), leftJustify, minWidth, precision, zeroPad, htmlSpace);
            case 'b': return formatBaseX(value, 2, prefixBaseX, leftJustify, minWidth, precision, zeroPad,htmlSpace);
            case 'o': return formatBaseX(value, 8, prefixBaseX, leftJustify, minWidth, precision, zeroPad, htmlSpace);
            case 'x': return formatBaseX(value, 16, prefixBaseX, leftJustify, minWidth, precision, zeroPad, htmlSpace);
            case 'X': return formatBaseX(value, 16, prefixBaseX, leftJustify, minWidth, precision, zeroPad, htmlSpace).toUpperCase();
            case 'u': return formatBaseX(value, 10, prefixBaseX, leftJustify, minWidth, precision, zeroPad, htmlSpace);
            case 'i': {
              var number = parseInt(+value, 10);
              if (isNaN(number)) {
                return '';
              }
              var prefix = number < 0 ? '-' : positivePrefix;
              value = prefix + pad(String(Math.abs(number)), precision, '0', false);
              return justify(value, prefix, leftJustify, minWidth, zeroPad, htmlSpace);
                  }
            case 'd': {
              var number = Math.round(+value);
              if (isNaN(number)) {
                return '';
              }
              var prefix = number < 0 ? '-' : positivePrefix;
              value = prefix + pad(String(Math.abs(number)), precision, '0', false);
              return justify(value, prefix, leftJustify, minWidth, zeroPad, htmlSpace);
                  }
            case 'e':
            case 'E':
            case 'f':
            case 'F':
            case 'g':
            case 'G':
                      {
                      var number = +value;
                      if (isNaN(number)) {
                          return '';
                      }
                      var prefix = number < 0 ? '-' : positivePrefix;
                      var method = ['toExponential', 'toFixed', 'toPrecision']['efg'.indexOf(type.toLowerCase())];
                      var textTransform = ['toString', 'toUpperCase']['eEfFgG'.indexOf(type) % 2];
                      value = prefix + Math.abs(number)[method](precision);
                      return justify(value, prefix, leftJustify, minWidth, zeroPad, htmlSpace)[textTransform]();
                  }
            case 'p':
            case 'P':
            {
                // make sure number is a number
                var number = +value;
                if (isNaN(number)) {
                    return '';
                }
                var prefix = number < 0 ? '-' : positivePrefix;

                var parts = String(Number(Math.abs(number)).toExponential()).split(/e|E/);
                var sd = (parts[0].indexOf('.') != -1) ? parts[0].length - 1 : parts[0].length;
                var zeros = (parts[1] < 0) ? -parts[1] - 1 : 0;
                
                if (Math.abs(number) < 1) {
                    if (sd + zeros  <= precision) {
                        value = prefix + Math.abs(number).toPrecision(sd);
                    }
                    else {
                        if (sd  <= precision - 1) {
                            value = prefix + Math.abs(number).toExponential(sd-1);
                        }
                        else {
                            value = prefix + Math.abs(number).toExponential(precision-1);
                        }
                    }
                }
                else {
                    var prec = (sd <= precision) ? sd : precision;
                    value = prefix + Math.abs(number).toPrecision(prec);
                }
                var textTransform = ['toString', 'toUpperCase']['pP'.indexOf(type) % 2];
                return justify(value, prefix, leftJustify, minWidth, zeroPad, htmlSpace)[textTransform]();
            }
            case 'n': return '';
            default: return substring;
            }
        });
    };
    
    $.jqplot.sprintf.regex = /%%|%(\d+\$)?([-+#0& ]*)(\*\d+\$|\*|\d+)?(\.(\*\d+\$|\*|\d+))?([nAscboxXuidfegpEGP])/g;

})(jQuery);  