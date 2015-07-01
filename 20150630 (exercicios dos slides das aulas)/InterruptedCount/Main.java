import java.util.ArrayList;
import java.util.Scanner;

public class Main {
	public static int count = 0;
	public static int limit;
	static int n = 10;

	public static void main(String[] args) {
		ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();
		Scanner in = new Scanner(System.in);
		limit = in.nextInt();
		in.close();

		for (int i = 0; i < n; i++) {
			ThreadSample ts = new ThreadSample();
			al.add(ts);
			ts.start();
		}
	}
}