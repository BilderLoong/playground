import LinkedList from '../Linked-List/Linked-List';

type hashNode = {
  key: string;
  value: unknown;
};

const defaultHashTableSize = 32;
export default class HashTable {
  buckets: LinkedList[];
  keys: object;
  constructor(size = defaultHashTableSize) {
    this.buckets = Array(size)
      .fill(null)
      .map(() => new LinkedList());

    this.keys = {};
  }

  hash(key: string) {
    const hash = Array.from(key).reduce((acc, char) => {
      return (acc as number) + (char as string).charCodeAt(0);
    }, 0);

    return hash % this.buckets.length;
  }

  set(key: string, value: number) {
    const keyHash = this.hash(key);
    this.keys[key] = keyHash;
    const bucketLinkedList = this.buckets[keyHash];

    const node = bucketLinkedList.find({
      callback: (value: hashNode) => value.key === key,
    });

    if (!node) {
      bucketLinkedList.append({ key, value });
    } else {
      (node.value as hashNode).value;
    }
  }
}
