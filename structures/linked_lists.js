class sll {

    constructor(args) {
        if (args.length === 0) {
            this.val = null;
            this.next = null;
        } else {
            this.val = args[0];
            this.next = new sll(args.slice(1));
        }
    }

    print() {
        if (this.val !== null) {
            console.log(this.val);
        }
        if (this.next !== null) {
            this.next.print();
        }
    }

    map(func) {
        if (this.val !== null) {
            this.val = func(this.val);
        }
        if (this.next !== null) {
            this.next.map(func);
        }
    }

    delete_n(n) {
        if (this.next === null || n < 0) {
            return;
        }
        if (n === 0) {
            this.val = this.next.val;
            this.next = this.next.next;
        } else {
            this.next.delete_n(n-1);
        }
    }

    insert(val) {
        const new_node = new sll([val]);
        new_node.next = this;
        return new_node;
    }

}

nodes = new sll([1, 2, 3, 4, 5]);
nodes = nodes.insert(9);
nodes.map((x) => (x * x));
nodes.delete_n(1);
nodes.print();

