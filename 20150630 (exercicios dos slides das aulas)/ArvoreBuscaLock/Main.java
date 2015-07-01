import java.util.ArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

// Relevant informations about this question
// - The question calls for 50 threads with 2000 inserts each
// - My Tree DO NOT insert a number which has already been inserted
// - To have a slightly chance of 100000 (2000*50) numbers inserted, the random needs to have range 0~100000
// - The output of the program crashes if the range of the random is greater than 1000

public class Main {
	public static Tree t = new Tree();

	public static void main(String[] args) {
		ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();

		for (int i = 0; i < 50; i++) {
			ThreadSample ts = new ThreadSample();
			al.add(ts);
			ts.start();
		}

		try {
			for (ThreadSample t : al) {
				t.join();
			}

			System.out.println(t.print());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

class Tree {
	int value;
	Tree esq, dir;
	Lock l = new ReentrantLock();

	public Tree() {
		this.value = -1;
		this.esq = null;
		this.dir = null;
	}

	public Tree(int val) {
		this.value = val;
		this.esq = null;
		this.dir = null;
	}

	public Tree insert(int value) {
		l.lock();

		try {
			if (this.value == -1) {
				this.value = value;
			} else if (this.value > value) {
				if (this.esq != null) {
					this.esq = this.esq.insert(value);
				} else {
					this.esq = new Tree(value);
				}
			} else if (this.value < value) {
				if (this.dir != null) {
					this.dir = this.dir.insert(value);
				} else {
					this.dir = new Tree(value);
				}
			}
		} finally {
			l.unlock();
		}

		return this;
	}

	public String print() {
		if (esq != null && dir != null) {
			return esq.print() + " " + value + " " + dir.print();
		} else if (esq != null && dir == null) {
			return esq.print() + " " + value;
		} else if (esq == null && dir != null) {
			return value + " " + dir.print();
		} else {
			return value + "";
		}
	}
}