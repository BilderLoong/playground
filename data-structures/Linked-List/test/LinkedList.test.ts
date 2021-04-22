import LinkedList from '../Linked-List';

describe('LinkedList', () => {
  test('should create empty linked list', () => {
    const linkedList = new LinkedList();
    expect(linkedList.toString()).toBe('');
  });
  it('should append node to linked list', () => {
    const linkedList = new LinkedList();

    expect(linkedList.head).toBeNull();
    expect(linkedList.tail).toBeNull();

    linkedList.append(1);
    linkedList.append(2);

    expect(linkedList.toString()).toBe('1,2');
    expect(linkedList.tail!.next).toBeNull();
  });

  it('should prepend node to linked list', () => {
    const linkedList = new LinkedList();

    linkedList.prepend(2);
    expect(linkedList.head!.toString()).toBe('2');
    expect(linkedList.tail!.toString()).toBe('2');

    linkedList.append(1);
    linkedList.prepend(3);

    expect(linkedList.toString()).toBe('3,2,1');
  });
  it('should delete node by value from linked list', () => {
    const linkedList = new LinkedList();

    expect(linkedList.delete(5)).toBeNull();

    linkedList.append(1);
    linkedList.append(1);
    linkedList.append(2);
    linkedList.append(3);
    linkedList.append(3);
    linkedList.append(3);
    linkedList.append(4);
    linkedList.append(5);

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('5');

    const deletedNode = linkedList.delete(3);
    expect(deletedNode!.value).toBe(3);
    expect(linkedList.toString()).toBe('1,1,2,4,5');

    linkedList.delete(3);
    expect(linkedList.toString()).toBe('1,1,2,4,5');

    linkedList.delete(1);
    expect(linkedList.toString()).toBe('2,4,5');

    expect(linkedList.head!.toString()).toBe('2');
    expect(linkedList.tail!.toString()).toBe('5');

    linkedList.delete(5);
    expect(linkedList.toString()).toBe('2,4');

    expect(linkedList.head!.toString()).toBe('2');
    expect(linkedList.tail!.toString()).toBe('4');

    linkedList.delete(4);
    expect(linkedList.toString()).toBe('2');

    expect(linkedList.head!.toString()).toBe('2');
    expect(linkedList.tail!.toString()).toBe('2');

    linkedList.delete(2);
    expect(linkedList.toString()).toBe('');
  });

  test('should delete one node of the given value', () => {
    const linkedList = new LinkedList();

    expect(linkedList.deleteOne(5)).toBeNull();

    linkedList.append(1);
    linkedList.append(1);
    linkedList.append(2);
    linkedList.append(3);
    linkedList.append(3);
    linkedList.append(3);
    linkedList.append(4);
    linkedList.append(5);

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('5');

    let deletedNode = linkedList.deleteOne(9);
    expect(deletedNode).toBeNull();
    expect(linkedList.toString()).toBe('1,1,2,3,3,3,4,5');

    deletedNode = linkedList.deleteOne(3);
    expect(deletedNode!.value).toBe(3);
    expect(linkedList.toString()).toBe('1,1,2,3,3,4,5');

    linkedList.deleteOne(3);
    expect(linkedList.toString()).toBe('1,1,2,3,4,5');

    linkedList.deleteOne(1);
    expect(linkedList.toString()).toBe('1,2,3,4,5');

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('5');

    linkedList.deleteOne(5);
    expect(linkedList.toString()).toBe('1,2,3,4');

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('4');

    linkedList.delete(4);
    expect(linkedList.toString()).toBe('1,2,3');

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('3');

    linkedList.delete(2);
    expect(linkedList.toString()).toBe('1,3');

    linkedList.deleteOne(3);
    expect(linkedList.toString()).toBe('1');

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('1');

    linkedList.deleteOne(1);
    expect(linkedList.toString()).toBe('');
  });

  test('should find node with the given value', () => {
    const linkedList = new LinkedList();

    expect(linkedList.deleteOne(5)).toBeNull();

    linkedList.append(1);
    linkedList.append(1);
    linkedList.append(2);
    linkedList.append(3);
    linkedList.append(3);
    linkedList.append(3);
    linkedList.append(4);
    linkedList.append(5);

    let node = linkedList.find(3);

    expect(node).toBeDefined();
    expect(node).not.toBeNull();
    expect(node!.value).toBe(3);

    node = linkedList.find(9);

    expect(node).toBeDefined();
    expect(node).toBeNull();
  });

  it('should delete linked list tail', () => {
    const linkedList = new LinkedList();

    linkedList.append(1);
    linkedList.append(2);
    linkedList.append(3);

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('3');

    const deletedNode1 = linkedList.deleteTail();

    expect(deletedNode1!.value).toBe(3);
    expect(linkedList.toString()).toBe('1,2');
    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('2');

    const deletedNode2 = linkedList.deleteTail();

    expect(deletedNode2!.value).toBe(2);
    expect(linkedList.toString()).toBe('1');
    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('1');

    const deletedNode3 = linkedList.deleteTail();

    expect(deletedNode3!.value).toBe(1);
    expect(linkedList.toString()).toBe('');
    expect(linkedList.head).toBeNull();
    expect(linkedList.tail).toBeNull();
  });

  it('should delete linked list head', () => {
    const linkedList = new LinkedList();

    expect(linkedList.deleteHead()).toBeNull();

    linkedList.append(1);
    linkedList.append(2);

    expect(linkedList.head!.toString()).toBe('1');
    expect(linkedList.tail!.toString()).toBe('2');

    const deletedNode1 = linkedList.deleteHead();

    expect(deletedNode1!.value).toBe(1);
    expect(linkedList.toString()).toBe('2');
    expect(linkedList.head!.toString()).toBe('2');
    expect(linkedList.tail!.toString()).toBe('2');

    const deletedNode2 = linkedList.deleteHead();

    expect(deletedNode2!.value).toBe(2);
    expect(linkedList.toString()).toBe('');
    expect(linkedList.head).toBeNull();
    expect(linkedList.tail).toBeNull();
  });

  it('should create linked list from array', () => {
    const linkedList = new LinkedList();
    linkedList.appendFromArray([1, 1, 2, 3, 3, 3, 4, 5]);

    expect(linkedList.toString()).toBe('1,1,2,3,3,3,4,5');
  });

  it('should reverse linked list', () => {
    const linkedList = new LinkedList();

    // Add test values to linked list.
    linkedList
      .append(1)
      .append(2)
      .append(3);

    expect(linkedList.toString()).toBe('1,2,3');
    expect(linkedList.head!.value).toBe(1);
    expect(linkedList.tail!.value).toBe(3);

    // Reverse linked list.
    linkedList.reverse();
    expect(linkedList.toString()).toBe('3,2,1');
    expect(linkedList.head!.value).toBe(3);
    expect(linkedList.tail!.value).toBe(1);

    // Reverse linked list back to initial state.
    linkedList.reverse();
    expect(linkedList.toString()).toBe('1,2,3');
    expect(linkedList.head!.value).toBe(1);
    expect(linkedList.tail!.value).toBe(3);
  });
});
