/*******************************************************************************
 * Setup
 ******************************************************************************/

interface BinaryTreeNode {
  value: number;
  left: BinaryTreeNode;
  right: BinaryTreeNode;
}

class BinaryTreeNode {
  constructor(value: number) {
    this.value = value;
    this.left  = null;
    this.right = null;
  }

  insertLeft(value: number): BinaryTreeNode {
    this.left = new BinaryTreeNode(value);
    return this.left;
  }

  insertRight(value: number): BinaryTreeNode {
    this.right = new BinaryTreeNode(value);
    return this.right;
  }
}

/*******************************************************************************
 * First solution
 ******************************************************************************/

/**
 * I first solved this problem using O(n) space and O(n*log(n))
 * time. InterviewCake informs me that we can improve both the time and the
 * space performance.
 */
function findSecondLargest_first(node: BinaryTreeNode): number {
  const stack: Array<BinaryTreeNode> = [];
  const xs: Array<number> = [];
  stack.push(node);

  while (stack.length > 0) {
    const node = stack.pop()

    xs.push(node.value);

    if (node.left) {
      stack.push(node.left);
    }
    if (node.right) {
      stack.push(node.right);
    }
  }

  xs.sort();

  if (xs.length < 2) {
    throw new Error('Cannot find the second largest element in a BST with fewer than two elements.');
  } else {
    return xs[xs.length - 2];
  }
}

/*******************************************************************************
 * Second solution
 ******************************************************************************/

/**
 * My second solution accumulates a list of the values in the tree using an
 * in-order traversal. This reduces the runtime costs from O(n*log(n)) from the
 * previous solution to O(n). The memory cost is still O(n), which InterviewCake
 * informs me can be reduced to O(1).
 */
function findSecondLargest_second(node: BinaryTreeNode): number {
  const xs: Array<number> = accumulateInorder(node);

  if (xs.length < 2) {
    throw new Error('Cannot find the second largest element in a BST with fewer than two elements.');
  } else {
    return xs[xs.length - 2];
  }
}

/**
 * Returns an array containing the values of the tree, `node`, sorted in-order
 * (i.e. from smallest-to-largest).
 */
function accumulateInorder(node: BinaryTreeNode): Array<number> {
  let result = [];

  if (node.left) {
    result = result.concat(accumulateInorder(node.left));
  }
  result.push(node.value)
  if (node.right) {
    result = result.concat(accumulateInorder(node.right));
  }

  return result;
}

/*******************************************************************************
 * Third solution
 ******************************************************************************/

/**
 * Returns the largest number in a BST.
 */
function findLargest(node: BinaryTreeNode): number {
  let curr: BinaryTreeNode = node;

  while (curr.right) {
    curr = curr.right;
  }

  return curr.value;
}

/**
 * Returns the second largest number in a BST
 */
function findSecondLargest(node: BinaryTreeNode): number {
  let curr = node;
  let parent = null;

  while (curr.right) {
    parent = curr;
    curr = curr.right
  }

  if (curr.left) {
    return findLargest(curr.left);
  }
  else {
    return parent.value;
  }
}


// Tests
let desc = 'full tree';
let treeRoot = new BinaryTreeNode(50);
let leftNode = treeRoot.insertLeft(30);
leftNode.insertLeft(10);
leftNode.insertRight(40);
let rightNode = treeRoot.insertRight(70);
rightNode.insertLeft(60);
rightNode.insertRight(80);
assertEquals(findSecondLargest(treeRoot), 70, desc);

desc = 'largest has a left child';
treeRoot = new BinaryTreeNode(50);
leftNode = treeRoot.insertLeft(30);
leftNode.insertLeft(10);
leftNode.insertRight(40);
rightNode = treeRoot.insertRight(70);
rightNode.insertLeft(60);
assertEquals(findSecondLargest(treeRoot), 60, desc);

desc = 'largest has a left subtree';
treeRoot = new BinaryTreeNode(50);
leftNode = treeRoot.insertLeft(30);
leftNode.insertLeft(10);
leftNode.insertRight(40);
rightNode = treeRoot.insertRight(70);
leftNode = rightNode.insertLeft(60);
leftNode.insertRight(65);
leftNode = leftNode.insertLeft(55);
leftNode.insertRight(58);
assertEquals(findSecondLargest(treeRoot), 65, desc);

desc = 'second largest is root node';
treeRoot = new BinaryTreeNode(50);
leftNode = treeRoot.insertLeft(30);
leftNode.insertLeft(10);
leftNode.insertRight(40);
rightNode = treeRoot.insertRight(70);
assertEquals(findSecondLargest(treeRoot), 50, desc);

desc = 'descending linked list';
treeRoot = new BinaryTreeNode(50);
leftNode = treeRoot.insertLeft(40);
leftNode = leftNode.insertLeft(30);
leftNode = leftNode.insertLeft(20);
leftNode = leftNode.insertLeft(10);
assertEquals(findSecondLargest(treeRoot), 40, desc);

desc = 'ascending linked list';
treeRoot = new BinaryTreeNode(50);
rightNode = treeRoot.insertRight(60);
rightNode = rightNode.insertRight(70);
rightNode = rightNode.insertRight(80);
assertEquals(findSecondLargest(treeRoot), 70, desc);

desc = 'one node tree';
treeRoot = new BinaryTreeNode(50);
assertThrowsError(() => findSecondLargest(treeRoot), desc);

desc = 'when tree is empty';
treeRoot = null;
assertThrowsError(() => findSecondLargest(treeRoot), desc);

function assertEquals(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`)
  }
}

function assertThrowsError(func, desc) {
  try {
    func();
    console.log(`${desc} ... FAIL`);
  } catch (e) {
    console.log(`${desc} ... PASS`);
  }
}
