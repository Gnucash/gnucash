/*!
 * chartjs-chart-sankey v0.1.5
 * https://github.com/kurkle/chartjs-chart-sankey#readme
 * (c) 2020 Jukka Kurkela
 * Released under the MIT license
 */
(function (global, factory) {
typeof exports === 'object' && typeof module !== 'undefined' ? factory(require('chart.js')) :
typeof define === 'function' && define.amd ? define(['chart.js'], factory) :
(global = global || self, factory(global.Chart));
}(this, (function (Chart) { 'use strict';

var Chart__default = 'default' in Chart ? Chart['default'] : Chart;

function _typeof(obj) {
  "@babel/helpers - typeof";

  if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
    _typeof = function (obj) {
      return typeof obj;
    };
  } else {
    _typeof = function (obj) {
      return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
    };
  }

  return _typeof(obj);
}

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

  return function _createSuperInternal() {
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

function _createForOfIteratorHelper(o, allowArrayLike) {
  var it;

  if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) {
    if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") {
      if (it) o = it;
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

  var normalCompletion = true,
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
    var column = nextColumn(_toConsumableArray(keys), to);

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

function nextColumn(keys, to) {
  var columnsNotInTo = keys.filter(function (key) {
    return !to.has(key);
  });
  return columnsNotInTo.length ? columnsNotInTo : keys.slice(0, 1);
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

var SankeyController = /*#__PURE__*/function (_DatasetController) {
  _inherits(SankeyController, _DatasetController);

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
SankeyController.id = 'sankey';
SankeyController.defaults = {
  dataElementType: 'flow',
  dataElementOptions: ['colorFrom', 'colorTo'],
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
      label: function label(context) {
        var item = context.dataset.data[context.dataIndex];
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

/*!
 * Chart.js v3.0.0-alpha.2
 * https://www.chartjs.org
 * (c) 2020 Chart.js Contributors
 * Released under the MIT License
 */

/*!
 * @kurkle/color v0.1.9
 * https://github.com/kurkle/color#readme
 * (c) 2020 Jukka Kurkela
 * Released under the MIT License
 */
var map = {
  0: 0,
  1: 1,
  2: 2,
  3: 3,
  4: 4,
  5: 5,
  6: 6,
  7: 7,
  8: 8,
  9: 9,
  A: 10,
  B: 11,
  C: 12,
  D: 13,
  E: 14,
  F: 15,
  a: 10,
  b: 11,
  c: 12,
  d: 13,
  e: 14,
  f: 15
};
var hex = '0123456789ABCDEF';

var h1 = function h1(b) {
  return hex[b & 0xF];
};

var h2 = function h2(b) {
  return hex[(b & 0xF0) >> 4] + hex[b & 0xF];
};

var eq = function eq(b) {
  return (b & 0xF0) >> 4 === (b & 0xF);
};

function isShort(v) {
  return eq(v.r) && eq(v.g) && eq(v.b) && eq(v.a);
}

function hexParse(str) {
  var len = str.length;
  var ret;

  if (str[0] === '#') {
    if (len === 4 || len === 5) {
      ret = {
        r: 255 & map[str[1]] * 17,
        g: 255 & map[str[2]] * 17,
        b: 255 & map[str[3]] * 17,
        a: len === 5 ? map[str[4]] * 17 : 255
      };
    } else if (len === 7 || len === 9) {
      ret = {
        r: map[str[1]] << 4 | map[str[2]],
        g: map[str[3]] << 4 | map[str[4]],
        b: map[str[5]] << 4 | map[str[6]],
        a: len === 9 ? map[str[7]] << 4 | map[str[8]] : 255
      };
    }
  }

  return ret;
}

function _hexString(v) {
  var f = isShort(v) ? h1 : h2;
  return v ? '#' + f(v.r) + f(v.g) + f(v.b) + (v.a < 255 ? f(v.a) : '') : v;
}

function round(v) {
  return v + 0.5 | 0;
}

var lim = function lim(v, l, h) {
  return Math.max(Math.min(v, h), l);
};

function p2b(v) {
  return lim(round(v * 2.55), 0, 255);
}

function n2b(v) {
  return lim(round(v * 255), 0, 255);
}

function b2n(v) {
  return lim(round(v / 2.55) / 100, 0, 1);
}

function n2p(v) {
  return lim(round(v * 100), 0, 100);
}

var RGB_RE = /^rgba?\(\s*([-+.\d]+)(%)?[\s,]+([-+.e\d]+)(%)?[\s,]+([-+.e\d]+)(%)?(?:[\s,/]+([-+.e\d]+)(%)?)?\s*\)$/;

function rgbParse(str) {
  var m = RGB_RE.exec(str);
  var a = 255;
  var r, g, b;

  if (!m) {
    return;
  }

  if (m[7] !== r) {
    var v = +m[7];
    a = 255 & (m[8] ? p2b(v) : v * 255);
  }

  r = +m[1];
  g = +m[3];
  b = +m[5];
  r = 255 & (m[2] ? p2b(r) : r);
  g = 255 & (m[4] ? p2b(g) : g);
  b = 255 & (m[6] ? p2b(b) : b);
  return {
    r: r,
    g: g,
    b: b,
    a: a
  };
}

function _rgbString(v) {
  return v && (v.a < 255 ? "rgba(".concat(v.r, ", ").concat(v.g, ", ").concat(v.b, ", ").concat(b2n(v.a), ")") : "rgb(".concat(v.r, ", ").concat(v.g, ", ").concat(v.b, ")"));
}

var HUE_RE = /^(hsla?|hwb|hsv)\(\s*([-+.e\d]+)(?:deg)?[\s,]+([-+.e\d]+)%[\s,]+([-+.e\d]+)%(?:[\s,]+([-+.e\d]+)(%)?)?\s*\)$/;

function hsl2rgbn(h, s, l) {
  var a = s * Math.min(l, 1 - l);

  var f = function f(n) {
    var k = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : (n + h / 30) % 12;
    return l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
  };

  return [f(0), f(8), f(4)];
}

function hsv2rgbn(h, s, v) {
  var f = function f(n) {
    var k = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : (n + h / 60) % 6;
    return v - v * s * Math.max(Math.min(k, 4 - k, 1), 0);
  };

  return [f(5), f(3), f(1)];
}

function hwb2rgbn(h, w, b) {
  var rgb = hsl2rgbn(h, 1, 0.5);
  var i;

  if (w + b > 1) {
    i = 1 / (w + b);
    w *= i;
    b *= i;
  }

  for (i = 0; i < 3; i++) {
    rgb[i] *= 1 - w - b;
    rgb[i] += w;
  }

  return rgb;
}

function rgb2hsl(v) {
  var range = 255;
  var r = v.r / range;
  var g = v.g / range;
  var b = v.b / range;
  var max = Math.max(r, g, b);
  var min = Math.min(r, g, b);
  var l = (max + min) / 2;
  var h, s, d;

  if (max !== min) {
    d = max - min;
    s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
    h = max === r ? (g - b) / d + (g < b ? 6 : 0) : max === g ? (b - r) / d + 2 : (r - g) / d + 4;
    h = h * 60 + 0.5;
  }

  return [h | 0, s || 0, l];
}

function calln(f, a, b, c) {
  return (Array.isArray(a) ? f(a[0], a[1], a[2]) : f(a, b, c)).map(n2b);
}

function hsl2rgb(h, s, l) {
  return calln(hsl2rgbn, h, s, l);
}

function hwb2rgb(h, w, b) {
  return calln(hwb2rgbn, h, w, b);
}

function hsv2rgb(h, s, v) {
  return calln(hsv2rgbn, h, s, v);
}

function hue(h) {
  return (h % 360 + 360) % 360;
}

function hueParse(str) {
  var m = HUE_RE.exec(str);
  var a = 255;
  var v;

  if (!m) {
    return;
  }

  if (m[5] !== v) {
    a = m[6] ? p2b(+m[5]) : n2b(+m[5]);
  }

  var h = hue(+m[2]);
  var p1 = +m[3] / 100;
  var p2 = +m[4] / 100;

  if (m[1] === 'hwb') {
    v = hwb2rgb(h, p1, p2);
  } else if (m[1] === 'hsv') {
    v = hsv2rgb(h, p1, p2);
  } else {
    v = hsl2rgb(h, p1, p2);
  }

  return {
    r: v[0],
    g: v[1],
    b: v[2],
    a: a
  };
}

function _rotate(v, deg) {
  var h = rgb2hsl(v);
  h[0] = hue(h[0] + deg);
  h = hsl2rgb(h);
  v.r = h[0];
  v.g = h[1];
  v.b = h[2];
}

function _hslString(v) {
  if (!v) {
    return;
  }

  var a = rgb2hsl(v);
  var h = a[0];
  var s = n2p(a[1]);
  var l = n2p(a[2]);
  return v.a < 255 ? "hsla(".concat(h, ", ").concat(s, "%, ").concat(l, "%, ").concat(b2n(v.a), ")") : "hsl(".concat(h, ", ").concat(s, "%, ").concat(l, "%)");
}

var map$1 = {
  x: 'dark',
  Z: 'light',
  Y: 're',
  X: 'blu',
  W: 'gr',
  V: 'medium',
  U: 'slate',
  A: 'ee',
  T: 'ol',
  S: 'or',
  B: 'ra',
  C: 'lateg',
  D: 'ights',
  R: 'in',
  Q: 'turquois',
  E: 'hi',
  P: 'ro',
  O: 'al',
  N: 'le',
  M: 'de',
  L: 'yello',
  F: 'en',
  K: 'ch',
  G: 'arks',
  H: 'ea',
  I: 'ightg',
  J: 'wh'
};
var names = {
  OiceXe: 'f0f8ff',
  antiquewEte: 'faebd7',
  aqua: 'ffff',
  aquamarRe: '7fffd4',
  azuY: 'f0ffff',
  beige: 'f5f5dc',
  bisque: 'ffe4c4',
  black: '0',
  blanKedOmond: 'ffebcd',
  Xe: 'ff',
  XeviTet: '8a2be2',
  bPwn: 'a52a2a',
  burlywood: 'deb887',
  caMtXe: '5f9ea0',
  KartYuse: '7fff00',
  KocTate: 'd2691e',
  cSO: 'ff7f50',
  cSnflowerXe: '6495ed',
  cSnsilk: 'fff8dc',
  crimson: 'dc143c',
  cyan: 'ffff',
  xXe: '8b',
  xcyan: '8b8b',
  xgTMnPd: 'b8860b',
  xWay: 'a9a9a9',
  xgYF: '6400',
  xgYy: 'a9a9a9',
  xkhaki: 'bdb76b',
  xmagFta: '8b008b',
  xTivegYF: '556b2f',
  xSange: 'ff8c00',
  xScEd: '9932cc',
  xYd: '8b0000',
  xsOmon: 'e9967a',
  xsHgYF: '8fbc8f',
  xUXe: '483d8b',
  xUWay: '2f4f4f',
  xUgYy: '2f4f4f',
  xQe: 'ced1',
  xviTet: '9400d3',
  dAppRk: 'ff1493',
  dApskyXe: 'bfff',
  dimWay: '696969',
  dimgYy: '696969',
  dodgerXe: '1e90ff',
  fiYbrick: 'b22222',
  flSOwEte: 'fffaf0',
  foYstWAn: '228b22',
  fuKsia: 'ff00ff',
  gaRsbSo: 'dcdcdc',
  ghostwEte: 'f8f8ff',
  gTd: 'ffd700',
  gTMnPd: 'daa520',
  Way: '808080',
  gYF: '8000',
  gYFLw: 'adff2f',
  gYy: '808080',
  honeyMw: 'f0fff0',
  hotpRk: 'ff69b4',
  RdianYd: 'cd5c5c',
  Rdigo: '4b0082',
  ivSy: 'fffff0',
  khaki: 'f0e68c',
  lavFMr: 'e6e6fa',
  lavFMrXsh: 'fff0f5',
  lawngYF: '7cfc00',
  NmoncEffon: 'fffacd',
  ZXe: 'add8e6',
  ZcSO: 'f08080',
  Zcyan: 'e0ffff',
  ZgTMnPdLw: 'fafad2',
  ZWay: 'd3d3d3',
  ZgYF: '90ee90',
  ZgYy: 'd3d3d3',
  ZpRk: 'ffb6c1',
  ZsOmon: 'ffa07a',
  ZsHgYF: '20b2aa',
  ZskyXe: '87cefa',
  ZUWay: '778899',
  ZUgYy: '778899',
  ZstAlXe: 'b0c4de',
  ZLw: 'ffffe0',
  lime: 'ff00',
  limegYF: '32cd32',
  lRF: 'faf0e6',
  magFta: 'ff00ff',
  maPon: '800000',
  VaquamarRe: '66cdaa',
  VXe: 'cd',
  VScEd: 'ba55d3',
  VpurpN: '9370db',
  VsHgYF: '3cb371',
  VUXe: '7b68ee',
  VsprRggYF: 'fa9a',
  VQe: '48d1cc',
  VviTetYd: 'c71585',
  midnightXe: '191970',
  mRtcYam: 'f5fffa',
  mistyPse: 'ffe4e1',
  moccasR: 'ffe4b5',
  navajowEte: 'ffdead',
  navy: '80',
  Tdlace: 'fdf5e6',
  Tive: '808000',
  TivedBb: '6b8e23',
  Sange: 'ffa500',
  SangeYd: 'ff4500',
  ScEd: 'da70d6',
  pOegTMnPd: 'eee8aa',
  pOegYF: '98fb98',
  pOeQe: 'afeeee',
  pOeviTetYd: 'db7093',
  papayawEp: 'ffefd5',
  pHKpuff: 'ffdab9',
  peru: 'cd853f',
  pRk: 'ffc0cb',
  plum: 'dda0dd',
  powMrXe: 'b0e0e6',
  purpN: '800080',
  YbeccapurpN: '663399',
  Yd: 'ff0000',
  Psybrown: 'bc8f8f',
  PyOXe: '4169e1',
  saddNbPwn: '8b4513',
  sOmon: 'fa8072',
  sandybPwn: 'f4a460',
  sHgYF: '2e8b57',
  sHshell: 'fff5ee',
  siFna: 'a0522d',
  silver: 'c0c0c0',
  skyXe: '87ceeb',
  UXe: '6a5acd',
  UWay: '708090',
  UgYy: '708090',
  snow: 'fffafa',
  sprRggYF: 'ff7f',
  stAlXe: '4682b4',
  tan: 'd2b48c',
  teO: '8080',
  tEstN: 'd8bfd8',
  tomato: 'ff6347',
  Qe: '40e0d0',
  viTet: 'ee82ee',
  JHt: 'f5deb3',
  wEte: 'ffffff',
  wEtesmoke: 'f5f5f5',
  Lw: 'ffff00',
  LwgYF: '9acd32'
};

function unpack() {
  var unpacked = {};
  var keys = Object.keys(names);
  var tkeys = Object.keys(map$1);
  var i, j, k, ok, nk;

  for (i = 0; i < keys.length; i++) {
    ok = nk = keys[i];

    for (j = 0; j < tkeys.length; j++) {
      k = tkeys[j];
      nk = nk.replace(k, map$1[k]);
    }

    k = parseInt(names[ok], 16);
    unpacked[nk] = [k >> 16 & 0xFF, k >> 8 & 0xFF, k & 0xFF];
  }

  return unpacked;
}

var names$1;

function nameParse(str) {
  if (!names$1) {
    names$1 = unpack();
    names$1.transparent = [0, 0, 0, 0];
  }

  var a = names$1[str.toLowerCase()];
  return a && {
    r: a[0],
    g: a[1],
    b: a[2],
    a: a.length === 4 ? a[3] : 255
  };
}

function modHSL(v, i, ratio) {
  if (v) {
    var tmp = rgb2hsl(v);
    tmp[i] = Math.max(0, Math.min(tmp[i] + tmp[i] * ratio, i === 0 ? 360 : 1));
    tmp = hsl2rgb(tmp);
    v.r = tmp[0];
    v.g = tmp[1];
    v.b = tmp[2];
  }
}

function clone(v, proto) {
  return v ? _extends(proto || {}, v) : v;
}

function fromObject(input) {
  var v = {
    r: 0,
    g: 0,
    b: 0,
    a: 255
  };

  if (Array.isArray(input)) {
    if (input.length >= 3) {
      v = {
        r: input[0],
        g: input[1],
        b: input[2],
        a: 255
      };

      if (input.length > 3) {
        v.a = n2b(input[3]);
      }
    }
  } else {
    v = clone(input, {
      r: 0,
      g: 0,
      b: 0,
      a: 1
    });
    v.a = n2b(v.a);
  }

  return v;
}

function functionParse(str) {
  if (str.charAt(0) === 'r') {
    return rgbParse(str);
  }

  return hueParse(str);
}

var Color = /*#__PURE__*/function () {
  function Color(input) {
    _classCallCheck(this, Color);

    if (input instanceof Color) {
      return input;
    }

    var type = _typeof(input);

    var v;

    if (type === 'object') {
      v = fromObject(input);
    } else if (type === 'string') {
      v = hexParse(input) || nameParse(input) || functionParse(input);
    }

    this._rgb = v;
    this._valid = !!v;
  }

  _createClass(Color, [{
    key: "rgbString",
    value: function rgbString() {
      return this._valid ? _rgbString(this._rgb) : this._rgb;
    }
  }, {
    key: "hexString",
    value: function hexString() {
      return this._valid ? _hexString(this._rgb) : this._rgb;
    }
  }, {
    key: "hslString",
    value: function hslString() {
      return this._valid ? _hslString(this._rgb) : this._rgb;
    }
  }, {
    key: "mix",
    value: function mix(color, weight) {
      var me = this;

      if (color) {
        var c1 = me.rgb;
        var c2 = color.rgb;
        var w2;
        var p = weight === w2 ? 0.5 : weight;
        var w = 2 * p - 1;
        var a = c1.a - c2.a;
        var w1 = ((w * a === -1 ? w : (w + a) / (1 + w * a)) + 1) / 2.0;
        w2 = 1 - w1;
        c1.r = 0xFF & w1 * c1.r + w2 * c2.r + 0.5;
        c1.g = 0xFF & w1 * c1.g + w2 * c2.g + 0.5;
        c1.b = 0xFF & w1 * c1.b + w2 * c2.b + 0.5;
        c1.a = p * c1.a + (1 - p) * c2.a;
        me.rgb = c1;
      }

      return me;
    }
  }, {
    key: "clone",
    value: function clone() {
      return new Color(this.rgb);
    }
  }, {
    key: "alpha",
    value: function alpha(a) {
      this._rgb.a = n2b(a);
      return this;
    }
  }, {
    key: "clearer",
    value: function clearer(ratio) {
      var rgb = this._rgb;
      rgb.a *= 1 - ratio;
      return this;
    }
  }, {
    key: "greyscale",
    value: function greyscale() {
      var rgb = this._rgb;
      var val = round(rgb.r * 0.3 + rgb.g * 0.59 + rgb.b * 0.11);
      rgb.r = rgb.g = rgb.b = val;
      return this;
    }
  }, {
    key: "opaquer",
    value: function opaquer(ratio) {
      var rgb = this._rgb;
      rgb.a *= 1 + ratio;
      return this;
    }
  }, {
    key: "negate",
    value: function negate() {
      var v = this._rgb;
      v.r = 255 - v.r;
      v.g = 255 - v.g;
      v.b = 255 - v.b;
      return this;
    }
  }, {
    key: "lighten",
    value: function lighten(ratio) {
      modHSL(this._rgb, 2, ratio);
      return this;
    }
  }, {
    key: "darken",
    value: function darken(ratio) {
      modHSL(this._rgb, 2, -ratio);
      return this;
    }
  }, {
    key: "saturate",
    value: function saturate(ratio) {
      modHSL(this._rgb, 1, ratio);
      return this;
    }
  }, {
    key: "desaturate",
    value: function desaturate(ratio) {
      modHSL(this._rgb, 1, -ratio);
      return this;
    }
  }, {
    key: "rotate",
    value: function rotate(deg) {
      _rotate(this._rgb, deg);

      return this;
    }
  }, {
    key: "valid",
    get: function get() {
      return this._valid;
    }
  }, {
    key: "rgb",
    get: function get() {
      var v = clone(this._rgb);

      if (v) {
        v.a = b2n(v.a);
      }

      return v;
    },
    set: function set(obj) {
      this._rgb = fromObject(obj);
    }
  }]);

  return Color;
}();

function index_esm(input) {
  return new Color(input);
}

var isPatternOrGradient = function isPatternOrGradient(value) {
  return value instanceof CanvasGradient || value instanceof CanvasPattern;
};

function color(value) {
  return isPatternOrGradient(value) ? value : index_esm(value);
}

var controlPoints = function controlPoints(x, y, x2, y2) {
  return x < x2 ? {
    cp1: {
      x: x + (x2 - x) / 3 * 2,
      y: y
    },
    cp2: {
      x: x + (x2 - x) / 3,
      y: y2
    }
  } : {
    cp1: {
      x: x - (x - x2) / 3,
      y: 0
    },
    cp2: {
      x: x2 + (x - x2) / 3,
      y: 0
    }
  };
};

var pointInLine = function pointInLine(p1, p2, t) {
  return {
    x: p1.x + t * (p2.x - p1.x),
    y: p1.y + t * (p2.y - p1.y)
  };
};

var Flow = /*#__PURE__*/function (_Element) {
  _inherits(Flow, _Element);

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
Flow.id = 'flow';
Flow.defaults = {
  colorFrom: 'red',
  colorTo: 'green'
};

Chart__default.register(SankeyController, Flow);

})));
