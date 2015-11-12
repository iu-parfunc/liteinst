
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.VolatileCallSite;
import java.util.Arrays;
import java.text.NumberFormat;

public class TimingHarness
{
   public static volatile boolean startSignal = false;
   public static volatile boolean stopSignal  = false;

   public static long numToggles = 0;

   public static void main(String[] args) throws Throwable
   {
      System.out.printf("Expects: num_runners duration target_rate\n");
      System.out.printf("Got: %s %s %s \n", args[0], args[1], args[2]);
      int num_runners = Integer.parseInt(args[0]);
      double duration = Double.parseDouble(args[1]);
      long target_rate = Long.parseLong(args[2]);

      long t1=0, t2=0, t3=0, t4=0;

      t1 = System.nanoTime ();
      t2 = System.nanoTime ();
      System.out.printf("Smallest time for System.nanotime, at first: %d\n", t2-t1);
      for(int i=0; i < 10000; i++) {
          t1 = System.nanoTime ();
          t2 = System.nanoTime ();
      }
      System.out.printf("And after some warmup: %d\n", t2-t1);

      int result = 0;

      // --------------------------------------------------------------------------------
      for(int i=0; i < IDDL.max_threads * IDDL.PAD; i++) {
           IDDL.f_calls[i] = 0;
           IDDL.g_calls[i] = 0;
      }

      // IDDL.f(); IDDL.g();
      IDD.main(null);

      // --------------------------------------------------------------------------------
      // Next for toggling:

      Runnable allRunnable[] = new Runnable[num_runners + 1];


      // Thread 0 toggles as fast as it can:
      allRunnable[0] = () -> {
          System.out.println("Toggler thread running.");
          try { Thread.sleep(100); // Wait a tenth of a second for everyone to get running.
          } catch (InterruptedException e) { }
          numToggles = 0;
          long duration_ns = (long)(duration * 1000000000);
          long startTime = System.nanoTime();
          long curTime = startTime;
          startSignal = true; // Tell the other threads to go.

          long current_toggles_per_s = 0;
          long elapsed_ns = 1;
          while (elapsed_ns < duration_ns) {
              current_toggles_per_s = numToggles * 1000000000 / elapsed_ns;

              long deficit = target_rate - current_toggles_per_s;
              if (deficit > 1000) deficit = 1000;

              boolean flip = true;
              for(; deficit > 0; deficit --) {
                  if (flip) {
                      IDDL.site.setTarget(IDDL.onMode);
                      flip = false;
                  } else {
                      IDDL.site.setTarget(IDDL.offMode);
                      flip = true;
                  }
                  numToggles++;
              }
              curTime = System.nanoTime();
              elapsed_ns = curTime - startTime;
          }
          stopSignal = true;
          System.out.format("Toggler thread, time slice finished: %d\n", curTime - startTime);
          System.out.format("Toggler thread, num toggles: %d\n", numToggles);
          System.out.format("Nanoseconds per toggle: %f\n", (curTime-startTime) / (double)numToggles);
      };

      // Thread 1-N runs the patch site as fast as possible
      for (int ind = 1; ind <= num_runners; ind ++) {
        final int ind_copy = ind;
        allRunnable[ind] = () -> {
            // IDDL.threadId.set(ind_copy); // Not working for some reason.
            while (! startSignal ) {}
            System.out.format("Executor thread %d thread running.\n", ind_copy);
            while (! stopSignal) {
                IDD.main(null);
            }
        };
      }

      Thread allThreads[] = new Thread[num_runners + 1];
      for (int i=0; i <= num_runners; i++) {
          allThreads[i] = new Thread( allRunnable[i] );
          allThreads[i].start();
      }

      for (int i=0; i <= num_runners; i++) {
          allThreads[i].join();
      }

      long f_total = 0;
      long g_total = 0;
      for (int i=0; i <= num_runners; i++) {
          f_total += IDDL.f_calls[i * IDDL.PAD];
          g_total += IDDL.g_calls[i * IDDL.PAD];
      }

      System.out.printf("All threads returned.\n");
      System.out.printf("  count of toggles: %s\n", NumberFormat.getNumberInstance().format(numToggles));
      System.out.printf("  count of f_calls: %s\n", NumberFormat.getNumberInstance().format(f_total));
      System.out.printf("  count of g_calls: %s\n", NumberFormat.getNumberInstance().format(g_total));

      // DecimalFormat formatter = new DecimalFormat("#,###.00");
      // System.out.println(formatter.format(numToggles));
      // NumberFormat.getInstance().format(numToggles);
      System.out.println();

      long total = IDDL.f_calls[0] + IDDL.g_calls[0];
      System.out.printf("  nanoseconds per call: %f", 1000000000.0 / (double)total );
   }
}
