import java.util.Random;

public class ThreadSample extends Thread {
	public void run() {
		Random r = new Random();
		int a;

		for (int i = 0; i < 20; i++) {
			a = r.nextInt(3);
			int index = r.nextInt(Main.v.vecList.length);

			if (a == 0) { // write
				int info = r.nextInt(100) + 1;
				Main.v.safeWrite(index, info);
			} else if (a == 1) { // read
				System.out.println(Main.v.safeRead(index));
			} else { // swap
				int index2 = r.nextInt(Main.v.vecList.length);
				Main.v.safeSwap(index, index2);
			}
		}
	}
}