/*!
 * chartjs-chart-sankey v0.1.5
 * https://github.com/kurkle/chartjs-chart-sankey#readme
 * (c) 2020 Jukka Kurkela
 * Released under the MIT license
 */
import { DatasetController, Element } from 'chart.js';

function calculateX(nodes, data) {
	const to = new Set(data.map(x => x.to));
	const from = new Set(data.map(x => x.from));
	const keys = new Set([...nodes.keys()]);
	let x = 0;
	while (keys.size) {
		const column = nextColumn([...keys], to);
		for (let i = 0; i < column.length; i++) {
			nodes.get(column[i]).x = x;
			keys.delete(column[i]);
		}
		if (keys.size) {
			to.clear();
			data.filter(flow => keys.has(flow.from)).forEach(flow => to.add(flow.to));
			x++;
		}
	}
	[...nodes.keys()]
		.filter(key => !from.has(key))
		.forEach(key => {
			nodes.get(key).x = x;
		});

	return x;
}

function nextColumn(keys, to) {
	const columnsNotInTo = keys.filter(key => !to.has(key));
	return columnsNotInTo.length ? columnsNotInTo : keys.slice(0, 1);
}

const nodeByXY = (a, b) => a.x !== b.x ? a.x - b.x : a.y - b.y;
const nodeCount = (list, prop) => list.reduce((acc, cur) => acc + cur.node[prop].length + nodeCount(cur.node[prop], prop), 0);
const flowByNodeCount = (prop) => (a, b) => nodeCount(a.node[prop], prop) - nodeCount(b.node[prop], prop);

function findLargestNode(nodeArray) {
	return nodeArray.sort((a, b) => Math.max(b.in, b.out) - Math.max(a.in, a.out))[0];
}

function processFrom(node, y) {
	node.from.sort(flowByNodeCount('from')).forEach(flow => {
		const n = flow.node;
		if (!('y' in n)) {
			n.y = y;
			y = Math.max(y + n.out, processFrom(n, y));
		}
	});
	return y;
}

function processTo(node, y) {
	node.to.sort(flowByNodeCount('to')).forEach(flow => {
		const n = flow.node;
		if (!('y' in n)) {
			n.y = y;
			y = Math.max(y + n.in, processTo(n, y));
		}
	});
	return y;
}

function processRest(nodeArray, maxX) {
	const leftNodes = nodeArray.filter(node => node.x === 0);
	const rightNodes = nodeArray.filter(node => node.x === maxX);

	let leftY = leftNodes.reduce((acc, cur) => Math.max(acc, (cur.y + cur.out) || 0), 0);
	let rightY = rightNodes.reduce((acc, cur) => Math.max(acc, (cur.y + cur.out) || 0), 0);

	if (leftY >= rightY) {
		leftNodes.forEach(n => {
			if (!('y' in n)) {
				n.y = leftY;
			} else {
				leftY = n.y;
			}
			leftY = Math.max(leftY + n.out, processTo(n, leftY));
		});

		rightNodes.forEach(n => {
			if (!('y' in n)) {
				n.y = rightY;
			} else {
				rightY = n.y;
			}
			rightY = Math.max(rightY + n.in, processTo(n, rightY));
		});
	} else {
		rightNodes.forEach(n => {
			if (!('y' in n)) {
				n.y = rightY;
			} else {
				rightY = n.y;
			}
			rightY = Math.max(rightY + n.in, processTo(n, rightY));
		});

		leftNodes.forEach(n => {
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
	const start = findLargestNode(nodeArray);
	start.y = 0;
	const left = processFrom(start, 0);
	const right = processTo(start, 0);
	const rest = processRest(nodeArray, maxX);
	return Math.max(left, right, rest);
}

function maxRows(nodeArray, maxX) {
	let max = 0;
	for (let i = 0; i <= maxX; i++) {
		max = Math.max(max, nodeArray.filter(n => n.x === i).length);
	}
	return max;
}

function addPadding(nodeArray, padding) {
	let i = 1;
	let x = 0;
	let prev = 0;
	const rows = [];
	nodeArray.sort(nodeByXY).forEach(node => {
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
	nodeArray.forEach(node => {
		let addY = 0;
		node.from.sort((a, b) => (a.node.y + a.node.out / 2) - (b.node.y + b.node.out / 2)).forEach(flow => {
			flow.addY = addY;
			addY += flow.flow;
		});
		addY = 0;
		node.to.sort((a, b) => (a.node.y + a.node.in / 2) - (b.node.y + b.node.in / 2)).forEach(flow => {
			flow.addY = addY;
			addY += flow.flow;
		});
	});
}

function layout(nodes, data) {
	const nodeArray = [...nodes.values()];
	const maxX = calculateX(nodes, data);
	const maxY = calculateY(nodeArray, maxX);
	const rows = maxRows(nodeArray, maxX);
	const padding = maxY * 0.03; // rows;

	addPadding(nodeArray, padding);
	sortFlows(nodeArray);

	return {maxX, maxY: maxY + rows * padding};
}

function buildNodesFromFlows(data) {
	const nodes = new Map();
	for (let i = 0; i < data.length; i++) {
		const d = data[i];
		if (!nodes.has(d.from)) {
			nodes.set(d.from, {key: d.from, in: 0, out: d.flow, from: [], to: [{key: d.to, flow: d.flow}]});
		} else {
			const node = nodes.get(d.from);
			node.out += d.flow;
			node.to.push({key: d.to, flow: d.flow});
		}
		if (!nodes.has(d.to)) {
			nodes.set(d.to, {key: d.to, in: d.flow, out: 0, from: [{key: d.from, flow: d.flow}], to: []});
		} else {
			const node = nodes.get(d.to);
			node.in += d.flow;
			node.from.push({key: d.from, flow: d.flow});
		}
	}

	const flowSort = (a, b) => b.flow - a.flow;

	[...nodes.values()].forEach(node => {
		let tmp = 0;
		node.from = node.from.sort(flowSort);
		node.from.forEach(x => {
			x.node = nodes.get(x.key);
			x.addY = tmp;
			tmp += x.flow;
		});

		tmp = 0;
		node.to = node.to.sort(flowSort);
		node.to.forEach(x => {
			x.node = nodes.get(x.key);
			x.addY = tmp;
			tmp += x.flow;
		});
	});

	return nodes;
}


function getAddY(arr, key) {
	for (let i = 0; i < arr.length; i++) {
		if (arr[i].key === key) {
			return arr[i].addY;
		}
	}
	return 0;
}

class SankeyController extends DatasetController {

	parseObjectData(meta, data, start, count) {
		const me = this;
		const {xScale, yScale} = meta;
		const parsed = [];
		const nodes = me._nodes = buildNodesFromFlows(data);

		const {maxX, maxY} = layout(nodes, data);

		xScale.options.max = maxX;
		yScale.options.max = maxY;

		for (let i = 0, ilen = data.length; i < ilen; ++i) {
			const flow = data[i];
			const from = nodes.get(flow.from);
			const to = nodes.get(flow.to);
			const fromY = from.y + getAddY(from.to, flow.to);
			const toY = to.y + getAddY(to.from, flow.from);
			parsed.push({
				x: xScale.parse(from.x, i),
				y: yScale.parse(fromY, i),
				_custom: {
					from,
					to,
					x: xScale.parse(to.x, i),
					y: yScale.parse(toY, i),
					height: yScale.parse(flow.flow, i),
				}
			});
		}
		return parsed.slice(start, start + count);
	}

	update(mode) {
		const me = this;
		const meta = me._cachedMeta;

		me.updateElements(meta.data, 0, mode);
	}

	updateElements(elems, start, mode) {
		const me = this;
		const {xScale, yScale} = me._cachedMeta;
		const firstOpts = me.resolveDataElementOptions(start, mode);
		const sharedOptions = me.getSharedOptions(mode, elems[start], firstOpts);

		for (let i = 0; i < elems.length; i++) {
			const index = start + i;
			const parsed = me.getParsed(index);
			const custom = parsed._custom;
			const y = yScale.getPixelForValue(parsed.y);
			me.updateElement(
				elems[i],
				index,
				{
					x: xScale.getPixelForValue(parsed.x) + 11,
					y,
					x2: xScale.getPixelForValue(custom.x) - 1,
					y2: yScale.getPixelForValue(custom.y),
					from: custom.from,
					to: custom.to,
					progress: mode === 'reset' ? 0 : 1,
					height: Math.abs(yScale.getPixelForValue(parsed.y + custom.height) - y),
					options: me.resolveDataElementOptions(i, mode)
				},
				mode);
		}

		me.updateSharedOptions(sharedOptions, mode);
	}

	_drawLabels() {
		const me = this;
		const ctx = me._ctx;
		const nodes = me._nodes || new Map();
		const {xScale, yScale} = me._cachedMeta;

		ctx.save();
		const chartArea = me.chart.chartArea;

		for (const node of nodes.values()) {
			const x = xScale.getPixelForValue(node.x);
			const y = yScale.getPixelForValue(node.y);
			const max = Math.max(node.in, node.out);
			const height = Math.abs(yScale.getPixelForValue(node.y + max) - y);
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
		ctx.restore();
	}

	_drawNodes() {
		const me = this;
		const ctx = me._ctx;
		const nodes = me._nodes || new Map();
		const {xScale, yScale} = me._cachedMeta;

		ctx.save();
		ctx.strokeStyle = 'black';

		for (const node of nodes.values()) {
			ctx.fillStyle = node.color;
			const x = xScale.getPixelForValue(node.x);
			const y = yScale.getPixelForValue(node.y);
			const max = Math.max(node.in, node.out);
			const height = Math.abs(yScale.getPixelForValue(node.y + max) - y);
			ctx.strokeRect(x, y, 10, height);
			ctx.fillRect(x, y, 10, height);
		}
		ctx.restore();
	}

	draw() {
		const me = this;
		const ctx = me._ctx;
		const data = me.getMeta().data || [];

		for (let i = 0, ilen = data.length; i < ilen; ++i) {
			const flow = data[i];
			flow.from.color = flow.options.colorFrom;
			flow.to.color = flow.options.colorTo;
		}

		me._drawLabels();
		me._drawNodes();

		for (let i = 0, ilen = data.length; i < ilen; ++i) {
			data[i].draw(ctx);
		}
	}
}

SankeyController.id = 'sankey';
SankeyController.defaults = {
	dataElementType: 'flow',
	dataElementOptions: [
		'colorFrom',
		'colorTo'
	],
	hover: {
		mode: 'nearest',
		intersect: true
	},
	datasets: {
		animation: (ctx) => {
			let delay = 0;
			let duration = 0;
			const parsed = ctx.chart.getDatasetMeta(ctx.datasetIndex).controller.getParsed(ctx.dataIndex);
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
					duration,
					delay
				},
				colors: {
					type: 'color',
					properties: ['colorFrom', 'colorTo'],
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
		color: () => '#efefef'
	},
	tooltips: {
		mode: 'nearest',
		intersect: true,
		callbacks: {
			title() {
				return '';
			},
			label(context) {
				const item = context.dataset.data[context.dataIndex];
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
const map = {0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9, A: 10, B: 11, C: 12, D: 13, E: 14, F: 15, a: 10, b: 11, c: 12, d: 13, e: 14, f: 15};
const hex = '0123456789ABCDEF';
const h1 = (b) => hex[b & 0xF];
const h2 = (b) => hex[(b & 0xF0) >> 4] + hex[b & 0xF];
const eq = (b) => (((b & 0xF0) >> 4) === (b & 0xF));
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
				a: len === 9 ? (map[str[7]] << 4 | map[str[8]]) : 255
			};
		}
	}
	return ret;
}
function hexString(v) {
	var f = isShort(v) ? h1 : h2;
	return v
		? '#' + f(v.r) + f(v.g) + f(v.b) + (v.a < 255 ? f(v.a) : '')
		: v;
}
function round(v) {
	return v + 0.5 | 0;
}
const lim = (v, l, h) => Math.max(Math.min(v, h), l);
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
const RGB_RE = /^rgba?\(\s*([-+.\d]+)(%)?[\s,]+([-+.e\d]+)(%)?[\s,]+([-+.e\d]+)(%)?(?:[\s,/]+([-+.e\d]+)(%)?)?\s*\)$/;
function rgbParse(str) {
	const m = RGB_RE.exec(str);
	let a = 255;
	let r, g, b;
	if (!m) {
		return;
	}
	if (m[7] !== r) {
		const v = +m[7];
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
function rgbString(v) {
	return v && (
		v.a < 255
			? `rgba(${v.r}, ${v.g}, ${v.b}, ${b2n(v.a)})`
			: `rgb(${v.r}, ${v.g}, ${v.b})`
	);
}
const HUE_RE = /^(hsla?|hwb|hsv)\(\s*([-+.e\d]+)(?:deg)?[\s,]+([-+.e\d]+)%[\s,]+([-+.e\d]+)%(?:[\s,]+([-+.e\d]+)(%)?)?\s*\)$/;
function hsl2rgbn(h, s, l) {
	const a = s * Math.min(l, 1 - l);
	const f = (n, k = (n + h / 30) % 12) => l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
	return [f(0), f(8), f(4)];
}
function hsv2rgbn(h, s, v) {
	const f = (n, k = (n + h / 60) % 6) => v - v * s * Math.max(Math.min(k, 4 - k, 1), 0);
	return [f(5), f(3), f(1)];
}
function hwb2rgbn(h, w, b) {
	const rgb = hsl2rgbn(h, 1, 0.5);
	let i;
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
	const range = 255;
	const r = v.r / range;
	const g = v.g / range;
	const b = v.b / range;
	const max = Math.max(r, g, b);
	const min = Math.min(r, g, b);
	const l = (max + min) / 2;
	let h, s, d;
	if (max !== min) {
		d = max - min;
		s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
		h = max === r
			? ((g - b) / d) + (g < b ? 6 : 0)
			: max === g
				? (b - r) / d + 2
				: (r - g) / d + 4;
		h = h * 60 + 0.5;
	}
	return [h | 0, s || 0, l];
}
function calln(f, a, b, c) {
	return (
		Array.isArray(a)
			? f(a[0], a[1], a[2])
			: f(a, b, c)
	).map(n2b);
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
	const m = HUE_RE.exec(str);
	let a = 255;
	let v;
	if (!m) {
		return;
	}
	if (m[5] !== v) {
		a = m[6] ? p2b(+m[5]) : n2b(+m[5]);
	}
	const h = hue(+m[2]);
	const p1 = +m[3] / 100;
	const p2 = +m[4] / 100;
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
function rotate(v, deg) {
	var h = rgb2hsl(v);
	h[0] = hue(h[0] + deg);
	h = hsl2rgb(h);
	v.r = h[0];
	v.g = h[1];
	v.b = h[2];
}
function hslString(v) {
	if (!v) {
		return;
	}
	const a = rgb2hsl(v);
	const h = a[0];
	const s = n2p(a[1]);
	const l = n2p(a[2]);
	return v.a < 255
		? `hsla(${h}, ${s}%, ${l}%, ${b2n(v.a)})`
		: `hsl(${h}, ${s}%, ${l}%)`;
}
const map$1 = {
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
const names = {
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
	const unpacked = {};
	const keys = Object.keys(names);
	const tkeys = Object.keys(map$1);
	let i, j, k, ok, nk;
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
let names$1;
function nameParse(str) {
	if (!names$1) {
		names$1 = unpack();
		names$1.transparent = [0, 0, 0, 0];
	}
	const a = names$1[str.toLowerCase()];
	return a && {
		r: a[0],
		g: a[1],
		b: a[2],
		a: a.length === 4 ? a[3] : 255
	};
}
function modHSL(v, i, ratio) {
	if (v) {
		let tmp = rgb2hsl(v);
		tmp[i] = Math.max(0, Math.min(tmp[i] + tmp[i] * ratio, i === 0 ? 360 : 1));
		tmp = hsl2rgb(tmp);
		v.r = tmp[0];
		v.g = tmp[1];
		v.b = tmp[2];
	}
}
function clone(v, proto) {
	return v ? Object.assign(proto || {}, v) : v;
}
function fromObject(input) {
	var v = {r: 0, g: 0, b: 0, a: 255};
	if (Array.isArray(input)) {
		if (input.length >= 3) {
			v = {r: input[0], g: input[1], b: input[2], a: 255};
			if (input.length > 3) {
				v.a = n2b(input[3]);
			}
		}
	} else {
		v = clone(input, {r: 0, g: 0, b: 0, a: 1});
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
class Color {
	constructor(input) {
		if (input instanceof Color) {
			return input;
		}
		const type = typeof input;
		let v;
		if (type === 'object') {
			v = fromObject(input);
		} else if (type === 'string') {
			v = hexParse(input) || nameParse(input) || functionParse(input);
		}
		this._rgb = v;
		this._valid = !!v;
	}
	get valid() {
		return this._valid;
	}
	get rgb() {
		var v = clone(this._rgb);
		if (v) {
			v.a = b2n(v.a);
		}
		return v;
	}
	set rgb(obj) {
		this._rgb = fromObject(obj);
	}
	rgbString() {
		return this._valid ? rgbString(this._rgb) : this._rgb;
	}
	hexString() {
		return this._valid ? hexString(this._rgb) : this._rgb;
	}
	hslString() {
		return this._valid ? hslString(this._rgb) : this._rgb;
	}
	mix(color, weight) {
		const me = this;
		if (color) {
			const c1 = me.rgb;
			const c2 = color.rgb;
			let w2;
			const p = weight === w2 ? 0.5 : weight;
			const w = 2 * p - 1;
			const a = c1.a - c2.a;
			const w1 = ((w * a === -1 ? w : (w + a) / (1 + w * a)) + 1) / 2.0;
			w2 = 1 - w1;
			c1.r = 0xFF & w1 * c1.r + w2 * c2.r + 0.5;
			c1.g = 0xFF & w1 * c1.g + w2 * c2.g + 0.5;
			c1.b = 0xFF & w1 * c1.b + w2 * c2.b + 0.5;
			c1.a = p * c1.a + (1 - p) * c2.a;
			me.rgb = c1;
		}
		return me;
	}
	clone() {
		return new Color(this.rgb);
	}
	alpha(a) {
		this._rgb.a = n2b(a);
		return this;
	}
	clearer(ratio) {
		const rgb = this._rgb;
		rgb.a *= 1 - ratio;
		return this;
	}
	greyscale() {
		const rgb = this._rgb;
		const val = round(rgb.r * 0.3 + rgb.g * 0.59 + rgb.b * 0.11);
		rgb.r = rgb.g = rgb.b = val;
		return this;
	}
	opaquer(ratio) {
		const rgb = this._rgb;
		rgb.a *= 1 + ratio;
		return this;
	}
	negate() {
		const v = this._rgb;
		v.r = 255 - v.r;
		v.g = 255 - v.g;
		v.b = 255 - v.b;
		return this;
	}
	lighten(ratio) {
		modHSL(this._rgb, 2, ratio);
		return this;
	}
	darken(ratio) {
		modHSL(this._rgb, 2, -ratio);
		return this;
	}
	saturate(ratio) {
		modHSL(this._rgb, 1, ratio);
		return this;
	}
	desaturate(ratio) {
		modHSL(this._rgb, 1, -ratio);
		return this;
	}
	rotate(deg) {
		rotate(this._rgb, deg);
		return this;
	}
}
function index_esm(input) {
	return new Color(input);
}

const isPatternOrGradient = (value) => value instanceof CanvasGradient || value instanceof CanvasPattern;
function color(value) {
	return isPatternOrGradient(value) ? value : index_esm(value);
}

const controlPoints = (x, y, x2, y2) => x < x2
	? {
		cp1: {x: x + (x2 - x) / 3 * 2, y},
		cp2: {x: x + (x2 - x) / 3, y: y2}
	}
	: {
		cp1: {x: x - (x - x2) / 3, y: 0},
		cp2: {x: x2 + (x - x2) / 3, y: 0}
	};

const pointInLine = (p1, p2, t) => ({x: p1.x + t * (p2.x - p1.x), y: p1.y + t * (p2.y - p1.y)});

class Flow extends Element {
	constructor(cfg) {
		super();

		this.options = undefined;
		this.x = undefined;
		this.y = undefined;
		this.x2 = undefined;
		this.y2 = undefined;
		this.height = undefined;

		if (cfg) {
			Object.assign(this, cfg);
		}
	}

	draw(ctx) {
		const me = this;
		const {x, x2, y, y2, height, progress} = me;
		const {cp1, cp2} = controlPoints(x, y, x2, y2);
		const options = me.options;

		if (progress === 0) {
			return;
		}
		ctx.save();
		if (progress < 1) {
			ctx.beginPath();
			ctx.rect(x, Math.min(y, y2), (x2 - x) * progress + 1, Math.abs(y2 - y) + height + 1);
			ctx.clip();
		}

		const fill = ctx.createLinearGradient(x, 0, x2, 0);
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

	inRange(mouseX, mouseY, useFinalPosition) {
		const {x, y, x2, y2, height} = this.getProps(['x', 'y', 'x2', 'y2', 'height'], useFinalPosition);
		if (mouseX < x || mouseX > x2) {
			return false;
		}
		const {cp1, cp2} = controlPoints(x, y, x2, y2);
		const t = (mouseX - x) / (x2 - x);
		const p1 = {x, y};
		const p2 = {x: x2, y: y2};
		const a = pointInLine(p1, cp1, t);
		const b = pointInLine(cp1, cp2, t);
		const c = pointInLine(cp2, p2, t);
		const d = pointInLine(a, b, t);
		const e = pointInLine(b, c, t);
		const topY = pointInLine(d, e, t).y;
		return mouseY >= topY && mouseY <= topY + height;
	}

	inXRange(mouseX, useFinalPosition) {
		const {x, x2} = this.getProps(['x', 'x2'], useFinalPosition);
		return mouseX >= x && mouseX <= x2;
	}

	inYRange(mouseY, useFinalPosition) {
		const {y, y2, height} = this.getProps(['y', 'y2', 'height'], useFinalPosition);
		const minY = Math.min(y, y2);
		const maxY = Math.max(y, y2) + height;
		return mouseY >= minY && mouseY <= maxY;
	}

	getCenterPoint(useFinalPosition) {
		const {x, y, x2, y2, height} = this.getProps(['x', 'y', 'x2', 'y2', 'height'], useFinalPosition);
		return {
			x: (x + x2) / 2,
			y: (y + y2 + height) / 2
		};
	}

	tooltipPosition() {
		return this.getCenterPoint();
	}

	getRange(axis) {
		return axis === 'x' ? this.width / 2 : this.height / 2;
	}
}

Flow.id = 'flow';
Flow.defaults = {
	colorFrom: 'red',
	colorTo: 'green'
};

export { Flow, SankeyController };
