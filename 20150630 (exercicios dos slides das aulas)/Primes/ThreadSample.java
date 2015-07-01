public class ThreadSample extends Thread {
	int i;

	public ThreadSample(int num) {
		this.i = num;
	}

	public void run() {
		boolean check = true;

		if (this.i >= 2) {
			for (int i = 2; i <= (this.i / 2); i++) {
				if (this.i % i == 0) {
					check = false;
				}
			}

			if (check && (this.i <= Main.n)) {
				ThreadPrincipal.retorno[this.i] = true;
			}
		}
	}
}
