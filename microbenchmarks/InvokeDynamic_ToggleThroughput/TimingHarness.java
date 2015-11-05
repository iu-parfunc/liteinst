
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.VolatileCallSite;
import java.util.Arrays;

public class TimingHarness
{
   private static int trials = 1000000;
   private static int trials_kept = 1000;
   private static long[] times = null;

   private static void summarize(String msg)
   {
       long mn = times[0];
       long mx = times[0];
       long sum = 0;
       for (long x : times) {
           mn = Math.min(mn,x);
           mx = Math.max(mx,x);
           sum += x;
       }
       Arrays.sort( times );
       long median = times[trials_kept / 2];
       System.out.printf("%s, min %d, max %d, avg %d, median %d\n", msg, mn,mx, sum / trials_kept, median);
       System.out.printf("  count of f_calls: %d\n", IDDL.f_calls);
       System.out.printf("  count of g_calls: %d\n", IDDL.g_calls);
   }

   public static void main(String[] args) throws Throwable
   {
      long t1=0, t2=0, t3=0, t4=0;
      times = new long[trials_kept];

      t1 = System.nanoTime ();
      t2 = System.nanoTime ();
      System.out.printf("Smallest time for System.nanotime, at first: %d\n", t2-t1);
      for(int i=0; i < 10000; i++) {
          t1 = System.nanoTime ();
          t2 = System.nanoTime ();
      }
      System.out.printf("And after some warmup: %d\n", t2-t1);


      //      System.out.printf("Calling IDD.main once...\n");
      //      IDD.main(null);

      // MethodHandles.Lookup lookup = MethodHandles.lookup();
      // MethodHandle mh = lookup.findStatic(InvokeDynamicTest2.class, "noop",
      //                                      MethodType.methodType(int.class, int.class));

      int result = 0;

      /*
      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDDL.g();
          t2 = System.nanoTime ();
          times[i % trials_kept] = t2-t1;
      }
      summarize("Time for direct call to g()");

      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDDL.f();
          t2 = System.nanoTime ();
          times[i % trials_kept] = t2-t1;
      }
      summarize("Time for direct call to f()");

      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDD.main(null);
          t2 = System.nanoTime ();
          times[i % trials_kept] = t2-t1;
      }
      summarize("Time for direct call to IDD.main() and then thru VolatileCallSite");
*/

      //      summarize("Starting state");

      IDDL.f();
      IDDL.g();
      IDD.main(null);

      // --------------------------------------------------------------------------------
      // Next for toggling:


      // Thread 0 toggles as fast as it can:
      Runnable thrd0 = () -> {
          System.out.println("Toggler thread running.");
          for (int i=0; i<trials; i++) {
              IDDL.site.setTarget(IDDL.onMode);
              IDDL.site.setTarget(IDDL.offMode);
          }
      };

      // Thread 1-N runs the patch site as fast as possible
      Runnable thrd1 = () -> {
          // int id = 1;
          System.out.format("executor thread %d thread running.\n", -1);
          for (int i=0; i<trials; i++) {
              IDD.main(null);
          }
      };

      Thread a = new Thread(thrd0);
      Thread b = new Thread(thrd1);

      a.start();
      b.start();

      a.join();
      b.join();

      summarize("All threads returned.");
   }
}
