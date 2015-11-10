
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.VolatileCallSite;
import java.util.Arrays;

public class TimingHarness
{
   public static volatile boolean startSignal = false;
   public static volatile boolean stopSignal  = false;

   public static void main(String[] args) throws Throwable
   {
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

      IDDL.f();
      IDDL.g();
      IDD.main(null);

      // --------------------------------------------------------------------------------
      // Next for toggling:

      // Thread 0 toggles as fast as it can:
      Runnable thrd0 = () -> {
          System.out.println("Toggler thread running.");
          long endTime = 0;
          long numToggles = 0;
          long startTime = System.nanoTime();
          startSignal = true;
          while (endTime < startTime + 1000000000) {
              IDDL.site.setTarget(IDDL.onMode);
              IDDL.site.setTarget(IDDL.offMode);
              numToggles += 2;
              endTime = System.nanoTime();
          }
          stopSignal = true;
          System.out.format("Toggler thread, time slice finished: %d\n", endTime - startTime);
          System.out.format("Toggler thread, num toggles: %d\n", numToggles);
          System.out.format("Nanoseconds per toggle: %f\n", (endTime-startTime) / (double)numToggles);
      };

      // Thread 1-N runs the patch site as fast as possible
      Runnable thrd1 = () -> {
          while (! startSignal ) {}
          // int id = 1;
          System.out.format("executor thread %d thread running.\n", -1);
          while (! stopSignal) {
              IDD.main(null);
          }
      };

      Thread a = new Thread(thrd0);
      Thread b = new Thread(thrd1);

      a.start();
      b.start();

      a.join();
      b.join();

      System.out.printf("All threads returned.\n");
      System.out.printf("  count of f_calls: %d\n", IDDL.f_calls);
      System.out.printf("  count of g_calls: %d\n", IDDL.g_calls);
      long total = IDDL.f_calls + IDDL.g_calls;
      System.out.printf("  nanoseconds per call: %f", 1000000000.0 / (double)total );
   }
}
