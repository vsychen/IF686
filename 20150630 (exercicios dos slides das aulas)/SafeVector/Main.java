import java.util.ArrayList;
import java.util.Scanner;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Main {
	public static Vector v;

	public static void main(String[] args) {
		ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();
		Scanner in = new Scanner(System.in);
		int length = in.nextInt();
		in.close();

		v = new Vector(length);

		for (int i = 0; i < 10; i++) {
			ThreadSample ts = new ThreadSample();
			al.add(ts);
			ts.start();
		}

		try {
			for (ThreadSample t : al) {
				t.join();
			}

			System.out.println(v.print());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

class Node {
	int value;
	int pos;
	Lock lock;

	public Node(int val, int pos) {
		this.value = val;
		this.pos = pos;
		this.lock = new ReentrantLock();
	}
}

class Vector {
	Node[] vecList;

	public Vector(int l) {
		this.vecList = new Node[l];

		for (int i = 0; i < l; i++) {
			this.vecList[i] = new Node(0, i);
		}
	}

	public void safeWrite(int index, int info) {
		synchronized (vecList[index]) {
			vecList[index].value = info;
		}
	}

	public int safeRead(int index) {
		synchronized (vecList[index]) {
			return vecList[index].value;
		}
	}

	public void safeSwap(int index1, int index2) {
		boolean lock1 = vecList[index1].lock.tryLock();
		boolean lock2 = vecList[index2].lock.tryLock();

		while (!(lock1 && lock2)) {
			if (lock1) {
				vecList[index1].lock.unlock();
			}

			if (lock2) {
				vecList[index2].lock.unlock();
			}

			lock1 = vecList[index1].lock.tryLock();
			lock2 = vecList[index2].lock.tryLock();
		}

		try {
			int auxiliar = 0;

			if (index1 != index2) {
				auxiliar = vecList[index1].value;
				vecList[index1].value = vecList[index2].value;
				vecList[index2].value = auxiliar;
			}
		} finally {
			if (lock1) {
				vecList[index1].lock.unlock();
			}

			if (lock2) {
				vecList[index2].lock.unlock();
			}
		}
	}

	public String print() {
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < this.vecList.length; i++) {
			sb.append(this.vecList[i].value);

			if (i != this.vecList.length - 1) {
				sb.append(" ");
			}
		}

		return sb.toString();
	}
}