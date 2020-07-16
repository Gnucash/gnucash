/*!
 * chartjs-chart-sankey v0.1.3
 * https://github.com/kurkle/chartjs-chart-sankey#readme
 * (c) 2020 Jukka Kurkela
 * Released under the MIT license
 */
(function (global, factory) {
typeof exports === 'object' && typeof module !== 'undefined' ? factory(require('chart.js')) :
typeof define === 'function' && define.amd ? define(['chart.js'], factory) :
(global = global || self, factory(global.Chart));
}(this, (function (Chart) { 'use strict';

Chart = Chart && Object.prototype.hasOwnProperty.call(Chart, 'default') ? Chart['default'] : Chart;

function _classCallCheck(instance, Constructor) {
  if (!(instance instanceof Constructor)) {
    throw new TypeError("Cannot call a class as a function");
  }
}

function _defineProperties(target, props) {
  for (var i = 0; i < props.length; i++) {
    var descriptor = props[i];
    descriptor.enumerable = descriptor.enumerable || false;
    descriptor.configurable = true;
    if ("value" in descriptor) descriptor.writable = true;
    Object.defineProperty(target, descriptor.key, descriptor);
  }
}

function _createClass(Constructor, protoProps, staticProps) {
  if (protoProps) _defineProperties(Constructor.prototype, protoProps);
  if (staticProps) _defineProperties(Constructor, staticProps);
  return Constructor;
}

function _defineProperty(obj, key, value) {
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }

  return obj;
}

function _extends() {
  _extends = Object.assign || function (target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = arguments[i];

      for (var key in source) {
        if (Object.prototype.hasOwnProperty.call(source, key)) {
          target[key] = source[key];
        }
      }
    }

    return target;
  };

  return _extends.apply(this, arguments);
}

function _inherits(subClass, superClass) {
  if (typeof superClass !== "function" && superClass !== null) {
    throw new TypeError("Super expression must either be null or a function");
  }

  subClass.prototype = Object.create(superClass && superClass.prototype, {
    constructor: {
      value: subClass,
      writable: true,
      configurable: true
    }
  });
  if (superClass) _setPrototypeOf(subClass, superClass);
}

function _getPrototypeOf(o) {
  _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) {
    return o.__proto__ || Object.getPrototypeOf(o);
  };
  return _getPrototypeOf(o);
}

function _setPrototypeOf(o, p) {
  _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) {
    o.__proto__ = p;
    return o;
  };

  return _setPrototypeOf(o, p);
}

function _isNativeReflectConstruct() {
  if (typeof Reflect === "undefined" || !Reflect.construct) return false;
  if (Reflect.construct.sham) return false;
  if (typeof Proxy === "function") return true;

  try {
    Date.prototype.toString.call(Reflect.construct(Date, [], function () {}));
    return true;
  } catch (e) {
    return false;
  }
}

function _assertThisInitialized(self) {
  if (self === void 0) {
    throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
  }

  return self;
}

function _possibleConstructorReturn(self, call) {
  if (call && (typeof call === "object" || typeof call === "function")) {
    return call;
  }

  return _assertThisInitialized(self);
}

function _createSuper(Derived) {
  var hasNativeReflectConstruct = _isNativeReflectConstruct();

  return function () {
    var Super = _getPrototypeOf(Derived),
        result;

    if (hasNativeReflectConstruct) {
      var NewTarget = _getPrototypeOf(this).constructor;

      result = Reflect.construct(Super, arguments, NewTarget);
    } else {
      result = Super.apply(this, arguments);
    }

    return _possibleConstructorReturn(this, result);
  };
}

function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread();
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}

function _iterableToArray(iter) {
  if (typeof Symbol !== "undefined" && Symbol.iterator in Object(iter)) return Array.from(iter);
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;

  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];

  return arr2;
}

function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

function _createForOfIteratorHelper(o) {
  if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) {
    if (Array.isArray(o) || (o = _unsupportedIterableToArray(o))) {
      var i = 0;

      var F = function () {};

      return {
        s: F,
        n: function () {
          if (i >= o.length) return {
            done: true
          };
          return {
            done: false,
            value: o[i++]
          };
        },
        e: function (e) {
          throw e;
        },
        f: F
      };
    }

    throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
  }

  var it,
      normalCompletion = true,
      didErr = false,
      err;
  return {
    s: function () {
      it = o[Symbol.iterator]();
    },
    n: function () {
      var step = it.next();
      normalCompletion = step.done;
      return step;
    },
    e: function (e) {
      didErr = true;
      err = e;
    },
    f: function () {
      try {
        if (!normalCompletion && it.return != null) it.return();
      } finally {
        if (didErr) throw err;
      }
    }
  };
}

var color = Chart.helpers.color;

var controlPoints = function controlPoints(x, y, x2, y2) {
  return {
    cp1: {
      x: x + (x2 - x) / 3 * 2,
      y: y
    },
    cp2: {
      x: x + (x2 - x) / 3,
      y: y2
    }
  };
};

var pointInLine = function pointInLine(p1, p2, t) {
  return {
    x: p1.x + t * (p2.x - p1.x),
    y: p1.y + t * (p2.y - p1.y)
  };
};

var Flow = /*#__PURE__*/function (_Chart$Element) {
  _inherits(Flow, _Chart$Element);

  var _super = _createSuper(Flow);

  function Flow(cfg) {
    var _this;

    _classCallCheck(this, Flow);

    _this = _super.call(this);
    _this.options = undefined;
    _this.x = undefined;
    _this.y = undefined;
    _this.x2 = undefined;
    _this.y2 = undefined;
    _this.height = undefined;

    if (cfg) {
      _extends(_assertThisInitialized(_this), cfg);
    }

    return _this;
  }

  _createClass(Flow, [{
    key: "draw",
    value: function draw(ctx) {
      var me = this;
      var x = me.x,
          x2 = me.x2,
          y = me.y,
          y2 = me.y2,
          height = me.height,
          progress = me.progress;

      var _controlPoints = controlPoints(x, y, x2, y2),
          cp1 = _controlPoints.cp1,
          cp2 = _controlPoints.cp2;

      var options = me.options;

      if (progress === 0) {
        return;
      }

      ctx.save();

      if (progress < 1) {
        ctx.beginPath();
        ctx.rect(x, Math.min(y, y2), (x2 - x) * progress + 1, Math.abs(y2 - y) + height + 1);
        ctx.clip();
      }

      var fill = ctx.createLinearGradient(x, 0, x2, 0);
      fill.addColorStop(0, color(options.colorFrom).alpha(0.5).rgbString());
      fill.addColorStop(1, color(options.colorTo).alpha(0.5).rgbString());
      ctx.fillStyle = fill;
      ctx.strokeStyle = fill;
      ctx.lineWidth = 0.5;
      ctx.beginPath();
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(cp1.x, cp1.y, cp2.x, cp2.y, x2, y2);
      ctx.lineTo(x2, y2 + height);
      ctx.bezierCurveTo(cp2.x, cp2.y + height, cp1.x, cp1.y + height, x, y + height);
      ctx.lineTo(x, y);
      ctx.stroke();
      ctx.closePath();
      ctx.fill();
      ctx.restore();
    }
  }, {
    key: "inRange",
    value: function inRange(mouseX, mouseY, useFinalPosition) {
      var _this$getProps = this.getProps(['x', 'y', 'x2', 'y2', 'height'], useFinalPosition),
          x = _this$getProps.x,
          y = _this$getProps.y,
          x2 = _this$getProps.x2,
          y2 = _this$getProps.y2,
          height = _this$getProps.height;

      if (mouseX < x || mouseX > x2) {
        return false;
      }

      var _controlPoints2 = controlPoints(x, y, x2, y2),
          cp1 = _controlPoints2.cp1,
          cp2 = _controlPoints2.cp2;

      var t = (mouseX - x) / (x2 - x);
      var p1 = {
        x: x,
        y: y
      };
      var p2 = {
        x: x2,
        y: y2
      };
      var a = pointInLine(p1, cp1, t);
      var b = pointInLine(cp1, cp2, t);
      var c = pointInLine(cp2, p2, t);
      var d = pointInLine(a, b, t);
      var e = pointInLine(b, c, t);
      var topY = pointInLine(d, e, t).y;
      return mouseY >= topY && mouseY <= topY + height;
    }
  }, {
    key: "inXRange",
    value: function inXRange(mouseX, useFinalPosition) {
      var _this$getProps2 = this.getProps(['x', 'x2'], useFinalPosition),
          x = _this$getProps2.x,
          x2 = _this$getProps2.x2;

      return mouseX >= x && mouseX <= x2;
    }
  }, {
    key: "inYRange",
    value: function inYRange(mouseY, useFinalPosition) {
      var _this$getProps3 = this.getProps(['y', 'y2', 'height'], useFinalPosition),
          y = _this$getProps3.y,
          y2 = _this$getProps3.y2,
          height = _this$getProps3.height;

      var minY = Math.min(y, y2);
      var maxY = Math.max(y, y2) + height;
      return mouseY >= minY && mouseY <= maxY;
    }
  }, {
    key: "getCenterPoint",
    value: function getCenterPoint(useFinalPosition) {
      var _this$getProps4 = this.getProps(['x', 'y', 'x2', 'y2', 'height'], useFinalPosition),
          x = _this$getProps4.x,
          y = _this$getProps4.y,
          x2 = _this$getProps4.x2,
          y2 = _this$getProps4.y2,
          height = _this$getProps4.height;

      return {
        x: (x + x2) / 2,
        y: (y + y2 + height) / 2
      };
    }
  }, {
    key: "tooltipPosition",
    value: function tooltipPosition() {
      return this.getCenterPoint();
    }
  }, {
    key: "getRange",
    value: function getRange(axis) {
      return axis === 'x' ? this.width / 2 : this.height / 2;
    }
  }]);

  return Flow;
}(Chart.Element);

_defineProperty(Flow, "_type", 'flow');

function calculateX(nodes, data) {
  var to = new Set(data.map(function (x) {
    return x.to;
  }));
  var from = new Set(data.map(function (x) {
    return x.from;
  }));
  var keys = new Set(_toConsumableArray(nodes.keys()));
  var x = 0;

  while (keys.size) {
    var column = _toConsumableArray(keys).filter(function (key) {
      return !to.has(key);
    });

    for (var i = 0; i < column.length; i++) {
      nodes.get(column[i]).x = x;
      keys["delete"](column[i]);
    }

    if (keys.size) {
      to.clear();
      data.filter(function (flow) {
        return keys.has(flow.from);
      }).forEach(function (flow) {
        return to.add(flow.to);
      });
      x++;
    }
  }

  _toConsumableArray(nodes.keys()).filter(function (key) {
    return !from.has(key);
  }).forEach(function (key) {
    nodes.get(key).x = x;
  });

  return x;
}

var nodeByXY = function nodeByXY(a, b) {
  return a.x !== b.x ? a.x - b.x : a.y - b.y;
};

var nodeCount = function nodeCount(list, prop) {
  return list.reduce(function (acc, cur) {
    return acc + cur.node[prop].length + nodeCount(cur.node[prop], prop);
  }, 0);
};

var flowByNodeCount = function flowByNodeCount(prop) {
  return function (a, b) {
    return nodeCount(a.node[prop], prop) - nodeCount(b.node[prop], prop);
  };
};

function findLargestNode(nodeArray) {
  return nodeArray.sort(function (a, b) {
    return Math.max(b["in"], b.out) - Math.max(a["in"], a.out);
  })[0];
}

function processFrom(node, y) {
  node.from.sort(flowByNodeCount('from')).forEach(function (flow) {
    var n = flow.node;

    if (!('y' in n)) {
      n.y = y;
      y = Math.max(y + n.out, processFrom(n, y));
    }
  });
  return y;
}

function processTo(node, y) {
  node.to.sort(flowByNodeCount('to')).forEach(function (flow) {
    var n = flow.node;

    if (!('y' in n)) {
      n.y = y;
      y = Math.max(y + n["in"], processTo(n, y));
    }
  });
  return y;
}

function processRest(nodeArray, maxX) {
  var leftNodes = nodeArray.filter(function (node) {
    return node.x === 0;
  });
  var rightNodes = nodeArray.filter(function (node) {
    return node.x === maxX;
  });
  var leftY = leftNodes.reduce(function (acc, cur) {
    return Math.max(acc, cur.y + cur.out || 0);
  }, 0);
  var rightY = rightNodes.reduce(function (acc, cur) {
    return Math.max(acc, cur.y + cur.out || 0);
  }, 0);

  if (leftY >= rightY) {
    leftNodes.forEach(function (n) {
      if (!('y' in n)) {
        n.y = leftY;
      } else {
        leftY = n.y;
      }

      leftY = Math.max(leftY + n.out, processTo(n, leftY));
    });
    rightNodes.forEach(function (n) {
      if (!('y' in n)) {
        n.y = rightY;
      } else {
        rightY = n.y;
      }

      rightY = Math.max(rightY + n["in"], processTo(n, rightY));
    });
  } else {
    rightNodes.forEach(function (n) {
      if (!('y' in n)) {
        n.y = rightY;
      } else {
        rightY = n.y;
      }

      rightY = Math.max(rightY + n["in"], processTo(n, rightY));
    });
    leftNodes.forEach(function (n) {
      if (!('y' in n)) {
        n.y = leftY;
      } else {
        leftY = n.y;
      }

      leftY = Math.max(leftY + n.out, processTo(n, leftY));
    });
  }

  return Math.max(leftY, rightY);
}

function calculateY(nodeArray, maxX) {
  var start = findLargestNode(nodeArray);
  start.y = 0;
  var left = processFrom(start, 0);
  var right = processTo(start, 0);
  var rest = processRest(nodeArray, maxX);
  return Math.max(left, right, rest);
}
function maxRows(nodeArray, maxX) {
  var max = 0;

  var _loop = function _loop(i) {
    max = Math.max(max, nodeArray.filter(function (n) {
      return n.x === i;
    }).length);
  };

  for (var i = 0; i <= maxX; i++) {
    _loop(i);
  }

  return max;
}
function addPadding(nodeArray, padding) {
  var i = 1;
  var x = 0;
  var prev = 0;
  var rows = [];
  nodeArray.sort(nodeByXY).forEach(function (node) {
    if (node.y) {
      if (node.x === 0) {
        rows.push(node.y);
      } else {
        if (x !== node.x) {
          x = node.x;
          prev = 0;
        }

        for (i = prev + 1; i < rows.length; i++) {
          if (rows[i] > node.y) {
            break;
          }
        }

        prev = i;
      }

      node.y += i * padding;
      i++;
    }
  });
}
function sortFlows(nodeArray) {
  nodeArray.forEach(function (node) {
    var addY = 0;
    node.from.sort(function (a, b) {
      return a.node.y + a.node.out / 2 - (b.node.y + b.node.out / 2);
    }).forEach(function (flow) {
      flow.addY = addY;
      addY += flow.flow;
    });
    addY = 0;
    node.to.sort(function (a, b) {
      return a.node.y + a.node["in"] / 2 - (b.node.y + b.node["in"] / 2);
    }).forEach(function (flow) {
      flow.addY = addY;
      addY += flow.flow;
    });
  });
}
function layout(nodes, data) {
  var nodeArray = _toConsumableArray(nodes.values());

  var maxX = calculateX(nodes, data);
  var maxY = calculateY(nodeArray, maxX);
  var rows = maxRows(nodeArray, maxX);
  var padding = maxY * 0.03; // rows;

  addPadding(nodeArray, padding);
  sortFlows(nodeArray);
  return {
    maxX: maxX,
    maxY: maxY + rows * padding
  };
}

function buildNodesFromFlows(data) {
  var nodes = new Map();

  for (var i = 0; i < data.length; i++) {
    var d = data[i];

    if (!nodes.has(d.from)) {
      nodes.set(d.from, {
        key: d.from,
        "in": 0,
        out: d.flow,
        from: [],
        to: [{
          key: d.to,
          flow: d.flow
        }]
      });
    } else {
      var node = nodes.get(d.from);
      node.out += d.flow;
      node.to.push({
        key: d.to,
        flow: d.flow
      });
    }

    if (!nodes.has(d.to)) {
      nodes.set(d.to, {
        key: d.to,
        "in": d.flow,
        out: 0,
        from: [{
          key: d.from,
          flow: d.flow
        }],
        to: []
      });
    } else {
      var _node = nodes.get(d.to);

      _node["in"] += d.flow;

      _node.from.push({
        key: d.from,
        flow: d.flow
      });
    }
  }

  var flowSort = function flowSort(a, b) {
    return b.flow - a.flow;
  };

  _toConsumableArray(nodes.values()).forEach(function (node) {
    var tmp = 0;
    node.from = node.from.sort(flowSort);
    node.from.forEach(function (x) {
      x.node = nodes.get(x.key);
      x.addY = tmp;
      tmp += x.flow;
    });
    tmp = 0;
    node.to = node.to.sort(flowSort);
    node.to.forEach(function (x) {
      x.node = nodes.get(x.key);
      x.addY = tmp;
      tmp += x.flow;
    });
  });

  return nodes;
}

function getAddY(arr, key) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i].key === key) {
      return arr[i].addY;
    }
  }

  return 0;
}

var SankeyController = /*#__PURE__*/function (_Chart$DatasetControl) {
  _inherits(SankeyController, _Chart$DatasetControl);

  var _super = _createSuper(SankeyController);

  function SankeyController() {
    _classCallCheck(this, SankeyController);

    return _super.apply(this, arguments);
  }

  _createClass(SankeyController, [{
    key: "parseObjectData",
    value: function parseObjectData(meta, data, start, count) {
      var me = this;
      var xScale = meta.xScale,
          yScale = meta.yScale;
      var parsed = [];
      var nodes = me._nodes = buildNodesFromFlows(data);

      var _layout = layout(nodes, data),
          maxX = _layout.maxX,
          maxY = _layout.maxY;

      xScale.options.max = maxX;
      yScale.options.max = maxY;

      for (var i = 0, ilen = data.length; i < ilen; ++i) {
        var flow = data[i];
        var from = nodes.get(flow.from);
        var to = nodes.get(flow.to);
        var fromY = from.y + getAddY(from.to, flow.to);
        var toY = to.y + getAddY(to.from, flow.from);
        parsed.push({
          x: xScale.parse(from.x, i),
          y: yScale.parse(fromY, i),
          _custom: {
            from: from,
            to: to,
            x: xScale.parse(to.x, i),
            y: yScale.parse(toY, i),
            height: yScale.parse(flow.flow, i)
          }
        });
      }

      return parsed.slice(start, start + count);
    }
  }, {
    key: "update",
    value: function update(mode) {
      var me = this;
      var meta = me._cachedMeta;
      me.updateElements(meta.data, 0, mode);
    }
  }, {
    key: "updateElements",
    value: function updateElements(elems, start, mode) {
      var me = this;
      var _me$_cachedMeta = me._cachedMeta,
          xScale = _me$_cachedMeta.xScale,
          yScale = _me$_cachedMeta.yScale;
      var firstOpts = me.resolveDataElementOptions(start, mode);
      var sharedOptions = me.getSharedOptions(mode, elems[start], firstOpts);

      for (var i = 0; i < elems.length; i++) {
        var index = start + i;
        var parsed = me.getParsed(index);
        var custom = parsed._custom;
        var y = yScale.getPixelForValue(parsed.y);
        me.updateElement(elems[i], index, {
          x: xScale.getPixelForValue(parsed.x) + 11,
          y: y,
          x2: xScale.getPixelForValue(custom.x) - 1,
          y2: yScale.getPixelForValue(custom.y),
          from: custom.from,
          to: custom.to,
          progress: mode === 'reset' ? 0 : 1,
          height: Math.abs(yScale.getPixelForValue(parsed.y + custom.height) - y),
          options: me.resolveDataElementOptions(i, mode)
        }, mode);
      }

      me.updateSharedOptions(sharedOptions, mode);
    }
  }, {
    key: "_drawLabels",
    value: function _drawLabels() {
      var me = this;
      var ctx = me._ctx;
      var nodes = me._nodes || new Map();
      var _me$_cachedMeta2 = me._cachedMeta,
          xScale = _me$_cachedMeta2.xScale,
          yScale = _me$_cachedMeta2.yScale;
      ctx.save();
      var chartArea = me.chart.chartArea;

      var _iterator = _createForOfIteratorHelper(nodes.values()),
          _step;

      try {
        for (_iterator.s(); !(_step = _iterator.n()).done;) {
          var node = _step.value;
          var x = xScale.getPixelForValue(node.x);
          var y = yScale.getPixelForValue(node.y);
          var max = Math.max(node["in"], node.out);
          var height = Math.abs(yScale.getPixelForValue(node.y + max) - y);
          ctx.fillStyle = 'black';
          ctx.textBaseline = 'middle';

          if (x < chartArea.width / 2) {
            ctx.textAlign = 'left';
            ctx.fillText(node.key, x + 15, y + height / 2);
          } else {
            ctx.textAlign = 'right';
            ctx.fillText(node.key, x - 5, y + height / 2);
          }
        }
      } catch (err) {
        _iterator.e(err);
      } finally {
        _iterator.f();
      }

      ctx.restore();
    }
  }, {
    key: "_drawNodes",
    value: function _drawNodes() {
      var me = this;
      var ctx = me._ctx;
      var nodes = me._nodes || new Map();
      var _me$_cachedMeta3 = me._cachedMeta,
          xScale = _me$_cachedMeta3.xScale,
          yScale = _me$_cachedMeta3.yScale;
      ctx.save();
      ctx.strokeStyle = 'black';

      var _iterator2 = _createForOfIteratorHelper(nodes.values()),
          _step2;

      try {
        for (_iterator2.s(); !(_step2 = _iterator2.n()).done;) {
          var node = _step2.value;
          ctx.fillStyle = node.color;
          var x = xScale.getPixelForValue(node.x);
          var y = yScale.getPixelForValue(node.y);
          var max = Math.max(node["in"], node.out);
          var height = Math.abs(yScale.getPixelForValue(node.y + max) - y);
          ctx.strokeRect(x, y, 10, height);
          ctx.fillRect(x, y, 10, height);
        }
      } catch (err) {
        _iterator2.e(err);
      } finally {
        _iterator2.f();
      }

      ctx.restore();
    }
  }, {
    key: "draw",
    value: function draw() {
      var me = this;
      var ctx = me._ctx;
      var data = me.getMeta().data || [];

      for (var i = 0, ilen = data.length; i < ilen; ++i) {
        var flow = data[i];
        flow.from.color = flow.options.colorFrom;
        flow.to.color = flow.options.colorTo;
      }

      me._drawLabels();

      me._drawNodes();

      for (var _i = 0, _ilen = data.length; _i < _ilen; ++_i) {
        data[_i].draw(ctx);
      }
    }
  }]);

  return SankeyController;
}(Chart.DatasetController);
SankeyController.prototype.dataElementType = Flow;
SankeyController.prototype.dataElementOptions = ['colorFrom', 'colorTo'];

Chart.controllers.sankey = SankeyController;
Chart.defaults.sankey = {
  hover: {
    mode: 'nearest',
    intersect: true
  },
  datasets: {
    animation: function animation(ctx) {
      var delay = 0;
      var duration = 0;
      var parsed = ctx.chart.getDatasetMeta(ctx.datasetIndex).controller.getParsed(ctx.dataIndex);

      if (parsed) {
        delay = parsed.x * 500 + ctx.dataIndex * 20;
        duration = (parsed._custom.x - parsed.x) * 200;
      }

      return {
        numbers: {
          type: 'number',
          properties: ['x', 'y', 'x2', 'y2', 'height']
        },
        progress: {
          easing: 'linear',
          duration: duration,
          delay: delay
        },
        colors: {
          type: 'color',
          properties: ['colorFrom', 'colorTo']
        },
        hide: {
          colors: {
            type: 'color',
            properties: ['colorFrom', 'colorTo'],
            to: 'transparent'
          }
        },
        show: {
          colors: {
            type: 'color',
            properties: ['colorFrom', 'colorTo'],
            from: 'transparent'
          }
        }
      };
    },
    color: function color() {
      return '#efefef';
    }
  },
  tooltips: {
    mode: 'nearest',
    intersect: true,
    callbacks: {
      title: function title() {
        return '';
      },
      label: function label(tooltipItem, data) {
        var item = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
        return item.from + ' -> ' + item.to + ': ' + item.flow;
      }
    }
  },
  legend: {
    display: false
  },
  scales: {
    x: {
      type: 'linear',
      display: false,
      min: 0,
      offset: true
    },
    y: {
      type: 'linear',
      display: false,
      min: 0,
      reverse: true,
      offset: true
    }
  }
};

})));
