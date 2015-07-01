import java.util.ArrayList;
import java.lang.Thread;

public class ThreadPrincipal extends Thread {
	public static boolean[] retorno;

	public void run() {
		int i = Main.i;
		ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();
		retorno = new boolean[i + 1];

		while (i > 0) {
			ThreadSample ts = new ThreadSample(i);
			ts.start();
			al.add(ts);
			i--;
		}

		try {
			for (ThreadSample t : al) {
				t.join();
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		for (int j = 0; j < retorno.length; j++) {
			if (retorno[j] == true) {
				System.out.println(j);
			}
		}
	}
}
