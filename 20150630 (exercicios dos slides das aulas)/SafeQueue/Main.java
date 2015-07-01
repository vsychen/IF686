import java.util.ArrayList;

public class Main {
	public static Queue q = new Queue();

	public static void main(String[] args) {
		ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();

		for (int i = 0; i < 10; i++) {
			ThreadSample ts = new ThreadSample();
			al.add(ts);
			ts.start();
		}

		try {
			for (ThreadSample t : al) {
				t.join();
			}

			System.out.println(q.print());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

class Queue {
	int value;
	Queue next;

	public Queue() {
		this.value = -1;
		this.next = null;
	}

	public Queue(int val) {
		this.value = val;
		this.next = null;
	}

	public synchronized Queue insert(int value) {
		if (this.value == -1) {
			this.value = value;
		} else if (this.next == null) {
			this.next = new Queue(value);
		} else {
			this.next = next.insert(value);
		}

		return this;
	}

	public String print() {
		if (next != null) {
			return value + " " + next.print();
		} else {
			return value + "";
		}
	}
}