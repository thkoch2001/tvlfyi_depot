type Color = string;

interface GraphNode {
  label: string;
  neighbors: Set<GraphNode>;
  color: string;
}

class GraphNode {
  constructor(label: string) {
    this.label = label;
    this.neighbors = new Set();
    this.color = null;
  }
}

interface Queue<A> {
  xs: Array<A>;
}

class Queue<A> {
  constructor() {
    this.xs = [];
  }
  isEmpty(): boolean {
    return this.xs.length === 0;
  }
  enqueue(x: A): void {
    this.xs.push(x);
  }
  dequeue(): A {
    return this.xs.shift();
  }
}

type Graph = Array<GraphNode>;

// Return a set of all of the colors from the neighbor nodes of `node`.
function neighborColors(node: GraphNode): Set<Color> {
  const result: Set<Color> = new Set();

  for (const x of node.neighbors) {
    if (typeof x.color === 'string') {
      result.add(x.color);
    }
  }

  return result;
}

// Returns the set difference between sets `xs`, and `ys`.
function setDifference<A>(xs: Set<A>, ys: Set<A>): Set<A> {
  const result: Set<A> = new Set();

  for (const x of xs) {
    if (!ys.has(x)) {
      result.add(x);
    }
  }

  return result;
}

// Returns an element from the set, `xs`.
// Throwns an error if `xs` is an empty set.
function choose<A>(xs: Set<A>): A {
  if (xs.size === 0) {
    throw new Error('Cannot choose an element from an empty set.');
  } else {
    return xs.values().next().value;
  }
}

// Returns true if `node` is present in `node.neighbors`.
function isCyclic(node: GraphNode): boolean {
  for (const x of node.neighbors) {
    if (x === node) {
      return true;
    }
  }
}

function colorGraph(graph: Graph, colors: Array<Color>): void {
  const allColors = new Set(colors);

  for (const node of graph) {
    if (isCyclic(node)) {
      throw new Error('InterviewCake would like me to invalidate this');
    }
    if (typeof node.color !== 'string') {
      node.color = choose(setDifference(allColors, neighborColors(node)));
    }
  }
}


// Tests
const colors = ['red', 'green', 'blue', 'orange', 'yellow', 'white'];

let graph = [];
{
  const nodeA = new GraphNode('A');
  const nodeB = new GraphNode('B');
  const nodeC = new GraphNode('C');
  const nodeD = new GraphNode('D');
  nodeA.neighbors.add(nodeB);
  nodeB.neighbors.add(nodeA);
  nodeB.neighbors.add(nodeC);
  nodeC.neighbors.add(nodeB);
  nodeC.neighbors.add(nodeD);
  nodeD.neighbors.add(nodeC);
  graph = [nodeA, nodeB, nodeC, nodeD];
}
colorGraph(graph, colors);
assertEqual(validateGraphColoring(graph), true, 'line graph');

{
  const nodeA = new GraphNode('A');
  const nodeB = new GraphNode('B');
  const nodeC = new GraphNode('C');
  const nodeD = new GraphNode('D');
  nodeA.neighbors.add(nodeB);
  nodeB.neighbors.add(nodeA);
  nodeC.neighbors.add(nodeD);
  nodeD.neighbors.add(nodeC);
  graph = [nodeA, nodeB, nodeC, nodeD];
}
colorGraph(graph, colors);
assertEqual(validateGraphColoring(graph), true, 'separate graph');

{
  const nodeA = new GraphNode('A');
  const nodeB = new GraphNode('B');
  const nodeC = new GraphNode('C');
  nodeA.neighbors.add(nodeB);
  nodeA.neighbors.add(nodeC);
  nodeB.neighbors.add(nodeA);
  nodeB.neighbors.add(nodeC);
  nodeC.neighbors.add(nodeA);
  nodeC.neighbors.add(nodeB);
  graph = [nodeA, nodeB, nodeC];
}
colorGraph(graph, colors);
assertEqual(validateGraphColoring(graph), true, 'triangle graph');

{
  const nodeA = new GraphNode('A');
  const nodeB = new GraphNode('B');
  const nodeC = new GraphNode('C');
  const nodeD = new GraphNode('D');
  const nodeE = new GraphNode('E');
  nodeA.neighbors.add(nodeB);
  nodeA.neighbors.add(nodeC);
  nodeB.neighbors.add(nodeA);
  nodeB.neighbors.add(nodeC);
  nodeB.neighbors.add(nodeD);
  nodeB.neighbors.add(nodeE);
  nodeC.neighbors.add(nodeA);
  nodeC.neighbors.add(nodeB);
  nodeC.neighbors.add(nodeD);
  nodeC.neighbors.add(nodeE);
  nodeD.neighbors.add(nodeB);
  nodeD.neighbors.add(nodeC);
  nodeD.neighbors.add(nodeE);
  nodeE.neighbors.add(nodeB);
  nodeE.neighbors.add(nodeC);
  nodeE.neighbors.add(nodeD);
  graph = [nodeA, nodeB, nodeC, nodeD, nodeE];
}
colorGraph(graph, colors);
assertEqual(validateGraphColoring(graph), true, 'envelope graph');

{
  const nodeA = new GraphNode('A');
  nodeA.neighbors.add(nodeA);
  graph = [nodeA];
}
assertThrows(() => {
  colorGraph(graph, colors);
}, 'loop graph');

function validateGraphColoring(graph) {

  const maxDegree = Math.max(...graph.map(node => node.neighbors.size));

  const colorsUsed = new Set();

  graph.forEach(node => {
    colorsUsed.add(node.color);
  });

  if (colorsUsed.has(null)) {
    return false;
  }

  if (colorsUsed.size > maxDegree + 1) {
    return false;
  }

  let badEdges = 0;

  graph.forEach(node => {
    node.neighbors.forEach(neighbor => {
      if (neighbor.color === node.color) {
        badEdges += 1;
      }
    });
  });

  if (badEdges > 0) {
    return false;
  }

  return true;
}

function assertEqual(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`);
  }
}

function assertThrows(func, desc) {
  try {
    func();
    console.log(`${desc} ... FAIL`);
  } catch (e) {
    console.log(`${desc} ... PASS`);
  }
}
