
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.VolatileCallSite;
import java.util.Arrays;
import java.text.NumberFormat;
import java.lang.Math;

public class TimingHarness
{
   public static volatile boolean startSignal = false;
   public static volatile boolean stopSignal  = false;

   public static long numToggles = 0;
   public static long elapsed_ns = 0;

   public static int toggler_thread_ind = -1;
   public static int main_thread_ind = -1;

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

      main_thread_ind = IDDL.threadId.get();
      System.out.printf("Main thread index is %d\n", main_thread_ind);

      // Thread 0 toggles as fast as it can:
      allRunnable[0] = () -> {
          System.out.println("Toggler thread running.");
          try { Thread.sleep(100); // Wait a tenth of a second for everyone to get running.
          } catch (InterruptedException e) { }
          numToggles = 0;
          toggler_thread_ind = IDDL.threadId.get();
          System.out.printf("Toggler thread index is %d\n", toggler_thread_ind);

          long duration_ns = (long)(duration * 1000000000);
          long startTime = System.nanoTime();
          long curTime = startTime;
          startSignal = true; // Tell the other threads to go.

          long current_toggles_per_s = 0;
          elapsed_ns = 1;
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
      long min_f_calls = Long.MAX_VALUE;
      long min_g_calls = Long.MAX_VALUE;
      long max_f_calls = 0;
      long max_g_calls = 0;
      for (int i=0; i < num_runners + 2; i++) {
        if (i == toggler_thread_ind || i == main_thread_ind) {
            System.out.printf("Not summing contribution from toggler thread %d\n", i);
        } else {
            long tmp_f = IDDL.f_calls[i * IDDL.PAD];
            long tmp_g = IDDL.g_calls[i * IDDL.PAD];
            System.out.printf("Thread %d totals: %d, %d\n", i, tmp_f, tmp_g);
            f_total += tmp_f;
            g_total += tmp_g;

            // HACK: ignore the zeros, because they're from the toggler thread:
            // if (tmp_f > 0)
                min_f_calls = Math.min(min_f_calls, tmp_f);
            // if (tmp_g > 0)
                min_g_calls = Math.min(min_g_calls, tmp_g);
            max_f_calls = Math.max(max_f_calls, IDDL.f_calls[i * IDDL.PAD]);
            max_g_calls = Math.max(max_g_calls, IDDL.g_calls[i * IDDL.PAD]);
        }
      }

      System.out.printf("All threads returned.\n");

      double elapsed_s = (double)elapsed_ns / 1000000000;

      System.out.printf("ALL COUNTS ARE REPORTED AS NUM/SEC\n");
      System.out.printf("STRADDLE_POINT: 0\n");
      // System.out.printf("MINIMUM_SWITCHES: %f\n", min_switches / t_diff);
      // System.out.printf("MAXIMUM_SWITCHES: %f\n", max_switches / t_diff);
      // System.out.printf("OBSERVED_SWITCHES_TOTAL: %f\n", observed_switches_total / t_diff);
      System.out.printf("MINIMUM_FOO_CALLS: %f\n", min_f_calls / elapsed_s);
      System.out.printf("MAXIMUM_FOO_CALLS: %f\n", max_f_calls / elapsed_s);
      System.out.printf("MINIMUM_BAR_CALLS: %f\n", min_g_calls / elapsed_s);
      System.out.printf("MAXIMUM_BAR_CALLS: %f\n", max_g_calls / elapsed_s);
      System.out.printf("NUMBER_OF_EXECUTERS: %d\n", num_runners);
      System.out.printf("TARGET_TIME: %f\n", duration);
      System.out.printf("ELAPSED_TIME: %f\n", elapsed_s);
      System.out.printf("NUMBER_OF_TOGGLES: %f\n", numToggles / elapsed_s);
      System.out.printf("TOTAL_FOO_CALLS: %f\n", f_total / elapsed_s);
      System.out.printf("TOTAL_BAR_CALLS: %f\n", g_total / elapsed_s);

      System.out.printf("\n Human readable output: \n");
      System.out.printf("  count of toggles: %s\n", NumberFormat.getNumberInstance().format(numToggles));
      System.out.printf("  count of f_calls: %s\n", NumberFormat.getNumberInstance().format(f_total));
      System.out.printf("  count of g_calls: %s\n", NumberFormat.getNumberInstance().format(g_total));
      System.out.printf("  combined calls: %s\n", NumberFormat.getNumberInstance().format(g_total+f_total));
      // System.out.printf("  nanoseconds per call: %f", 1000000000.0 / (double)total );
   }
}
