import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MutableCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public class IDDL
{
   public static MethodHandle onMode;
   public static MethodHandle offMode;
   static public MutableCallSite site = null;

   static public long active_calls = 0;

   // --------------------------------------------------------------------------------
   // Our two function calls to toggle between:

   public static void active_call()
   {
      active_calls ++;
   }

   public static void noop() {
   }

   // --------------------------------------------------------------------------------

   public static CallSite bootstrapDynamic(MethodHandles.Lookup caller,
                                           String name,
                                           MethodType type)
      throws IllegalAccessException, NoSuchMethodException
   {
      MethodHandles.Lookup lookup = MethodHandles.lookup();
      Class thisClass = lookup.lookupClass();
      onMode = lookup.findStatic(thisClass, "active_call",
                             MethodType.methodType(void.class));
      if (!type.equals(onMode.type()))
         onMode = onMode.asType(type);

      offMode = lookup.findStatic(thisClass, "noop",
                             MethodType.methodType(void.class));
      if (!type.equals(offMode.type()))
         offMode = offMode.asType(type);

      site = new MutableCallSite(onMode);

      System.out.println("BOOTSTRAPPING callsite " + site.toString() );

      return site;
   }

    public static void changeSite()
    {
        System.out.printf("Mutating call site...\n");
        //   site.
    }


}
