import LinkedListNode from '../Linked-List-Node';

describe('LinkedListNode', () => {
  test('should create a new node', () => {
    const node = new LinkedListNode(1);
    expect(node.value).toBe(1);
    expect(node.next).toBeNull();
  });

  test('should create a node with an object value', () => {
    const node = new LinkedListNode({ val: 1, key: 'key' });

    expect((node.value as any).val).toBe(1);
    expect((node.value as any).key).toBe('key');
    expect(node.next).toBeNull();
  });

  test('should be linked together', () => {
    const node1 = new LinkedListNode({ val: 1, key: 'key' });
    const node2 = new LinkedListNode(1);
    node1.next = node2;

    expect(node1.next).toBeDefined();
    expect(node1.next).toBe(node2);
    expect(node1.next.value).toBe(1);
    expect(node2.next).toBeNull();
  });

  test('should to be a string', () => {
    const node = new LinkedListNode(1);
    expect((node.value as any).toString()).toBe('1');
  });

  it('should convert node to string with custom stringifier', () => {
    const nodeValue = { value: 1, key: 'test' };
    const node = new LinkedListNode(nodeValue);
    const toStringCallback = (value:any) =>
      `value: ${value.value}, key: ${value.key}`;

    expect(node.toString(toStringCallback)).toBe('value: 1, key: test');
  });
});
