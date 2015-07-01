public class ThreadSample extends Thread {
	public void run() {
		while (Main.count <= Main.limit) {
			System.out.println(Main.count++);

			try {
				if (interrupted()) {
					return;
				}

				sleep(1);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

		if (Main.count > Main.limit) {
			interrupt();
		}
	}
}