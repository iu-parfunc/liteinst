
/*
  Benchmark description:
  Measures average probe activation and deactivation costs of a probe via Finstrumentor ProbeProvider API.
   */

#include "liteinst.hpp"
#include "process.hpp"
#include "cycle.h"
#include "funcs.hpp"

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;
using namespace utils::process;
using namespace liteinst;

#define OUTLIER 1000000

long invoke_count = 0;
long invoke_exit_count = 0;
long max_probe_id = 0;
long DEFAULT_FUNC_ID = 1;
long NUM_FUNCS = 10;

ticks* deactivation_costs;
ticks* activation_costs;

/*
void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));
*/ 

/* void instrumentation(ProbeArg func_id) { */
/*   invoke_count++; */
/* } */

void instrumentation() {
  invoke_count++;
}

void instrumentation_exit() {
  invoke_exit_count++;
}


/* void callback(const ProbeMetaData* pmd) { */

/*   // printf("probe id : %lu\n", pmd->probe_id); */
/*   assert(pmd->probe_id == 0 || pmd->probe_id == max_probe_id + 1); */
/*   // if(pmd->probe_id > max_probe_id) { */
/*     max_probe_id = pmd->probe_id; */
/*   // } */

/*   PROBE_PROVIDER->initialize(pmd->probe_id, DEFAULT_FUNC_ID); */
/*   try { */
/*     PROBE_PROVIDER->activate(pmd->probe_id, instrumentation); */
/*   } catch (int e) { */
/*     fprintf(stderr, "Error while activating probe of function : %s.\n", */
/*         pmd->func_name.c_str()); */
/*     exit(EXIT_FAILURE); */
/*   } */

/* } */

void callback(const ProbeInfo* pi) {

  /* // printf("probe id : %lu\n", pmd->probe_id); */
  /* assert(pmd->probe_id == 0 || pmd->probe_id == max_probe_id + 1); */
  /* // if(pmd->probe_id > max_probe_id) { */
  /*   max_probe_id = pmd->probe_id; */
  /* // } */

  /* PROBE_PROVIDER->initialize(pmd->probe_id, DEFAULT_FUNC_ID); */
  /* try { */
  /*   PROBE_PROVIDER->activate(pmd->probe_id, instrumentation); */
  /* } catch (int e) { */
  /*   fprintf(stderr, "Error while activating probe of function : %s.\n", */
  /*       pmd->func_name.c_str()); */
  /*   exit(EXIT_FAILURE); */
  /* } */

}


int main(int argc, char* argv[]) {


  /*  fprintf(stderr, "Benchmark probe initialization..\n"); */

  if (argc == 1) {
    printf("NO ARGS: Running with default NUM_FUNCS : %ld..\n", NUM_FUNCS); 
  } else {
    NUM_FUNCS = atoi(argv[1]);
    printf("Running with NUM_FUNCS : %ld..\n", NUM_FUNCS); 
  }

  ProbeProvider* p;

  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES,nullptr,nullptr); 

  InstrumentationProvider i_provider("instr", instrumentation, instrumentation_exit);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");
  
  Coordinates coords;
  // coords.setFunction(liteinst::Function("*~_ZnwmPv/.*instrumentation.*"));
  coords.setFunction(liteinst::Function(".*emptyFunc.*"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "instr"); 
  
  Process process; 

  printf("getProbedFunctions: %d\n", pr.getProbedFunctions().size()); 
  max_probe_id = pr.getProbedFunctions().size();
  emptyFunc0();
  emptyFunc1();
  emptyFunc2();
  emptyFunc3();
  emptyFunc4();
  emptyFunc5();
  emptyFunc6();
  emptyFunc7();
  emptyFunc8();
  emptyFunc9();
  emptyFunc10();
  emptyFunc11();
  emptyFunc12();
  emptyFunc13();
  emptyFunc14();
  emptyFunc15();
  emptyFunc16();
  emptyFunc17();
  emptyFunc18();
  emptyFunc19();
  emptyFunc20();
  emptyFunc21();
  emptyFunc22();
  emptyFunc23();
  emptyFunc24();
  emptyFunc25();
  emptyFunc26();
  emptyFunc27();
  emptyFunc28();
  emptyFunc29();
  emptyFunc30();
  emptyFunc31();
  emptyFunc32();
  emptyFunc33();
  emptyFunc34();
  emptyFunc35();
  emptyFunc36();
  emptyFunc37();
  emptyFunc38();
  emptyFunc39();
  emptyFunc40();
  emptyFunc41();
  emptyFunc42();
  emptyFunc43();
  emptyFunc44();
  emptyFunc45();
  emptyFunc46();
  emptyFunc47();
  emptyFunc48();
  emptyFunc49();
  emptyFunc50();
  emptyFunc51();
  emptyFunc52();
  emptyFunc53();
  emptyFunc54();
  emptyFunc55();
  emptyFunc56();
  emptyFunc57();
  emptyFunc58();
  emptyFunc59();
  emptyFunc60();
  emptyFunc61();
  emptyFunc62();
  emptyFunc63();
  emptyFunc64();
  emptyFunc65();
  emptyFunc66();
  emptyFunc67();
  emptyFunc68();
  emptyFunc69();
  emptyFunc70();
  emptyFunc71();
  emptyFunc72();
  emptyFunc73();
  emptyFunc74();
  emptyFunc75();
  emptyFunc76();
  emptyFunc77();
  emptyFunc78();
  emptyFunc79();
  emptyFunc80();
  emptyFunc81();
  emptyFunc82();
  emptyFunc83();
  emptyFunc84();
  emptyFunc85();
  emptyFunc86();
  emptyFunc87();
  emptyFunc88();
  emptyFunc89();
  emptyFunc90();
  emptyFunc91();
  emptyFunc92();
  emptyFunc93();
  emptyFunc94();
  emptyFunc95();
  emptyFunc96();
  emptyFunc97();
  emptyFunc98();
  emptyFunc99();
  emptyFunc100();
  emptyFunc101();
  emptyFunc102();
  emptyFunc103();
  emptyFunc104();
  emptyFunc105();
  emptyFunc106();
  emptyFunc107();
  emptyFunc108();
  emptyFunc109();
  emptyFunc110();
  emptyFunc111();
  emptyFunc112();
  emptyFunc113();
  emptyFunc114();
  emptyFunc115();
  emptyFunc116();
  emptyFunc117();
  emptyFunc118();
  emptyFunc119();
  emptyFunc120();
  emptyFunc121();
  emptyFunc122();
  emptyFunc123();
  emptyFunc124();
  emptyFunc125();
  emptyFunc126();
  emptyFunc127();
  emptyFunc128();
  emptyFunc129();
  emptyFunc130();
  emptyFunc131();
  emptyFunc132();
  emptyFunc133();
  emptyFunc134();
  emptyFunc135();
  emptyFunc136();
  emptyFunc137();
  emptyFunc138();
  emptyFunc139();
  emptyFunc140();
  emptyFunc141();
  emptyFunc142();
  emptyFunc143();
  emptyFunc144();
  emptyFunc145();
  emptyFunc146();
  emptyFunc147();
  emptyFunc148();
  emptyFunc149();
  emptyFunc150();
  emptyFunc151();
  emptyFunc152();
  emptyFunc153();
  emptyFunc154();
  emptyFunc155();
  emptyFunc156();
  emptyFunc157();
  emptyFunc158();
  emptyFunc159();
  emptyFunc160();
  emptyFunc161();
  emptyFunc162();
  emptyFunc163();
  emptyFunc164();
  emptyFunc165();
  emptyFunc166();
  emptyFunc167();
  emptyFunc168();
  emptyFunc169();
  emptyFunc170();
  emptyFunc171();
  emptyFunc172();
  emptyFunc173();
  emptyFunc174();
  emptyFunc175();
  emptyFunc176();
  emptyFunc177();
  emptyFunc178();
  emptyFunc179();
  emptyFunc180();
  emptyFunc181();
  emptyFunc182();
  emptyFunc183();
  emptyFunc184();
  emptyFunc185();
  emptyFunc186();
  emptyFunc187();
  emptyFunc188();
  emptyFunc189();
  emptyFunc190();
  emptyFunc191();
  emptyFunc192();
  emptyFunc193();
  emptyFunc194();
  emptyFunc195();
  emptyFunc196();
  emptyFunc197();
  emptyFunc198();
  emptyFunc199();
  emptyFunc200();
  emptyFunc201();
  emptyFunc202();
  emptyFunc203();
  emptyFunc204();
  emptyFunc205();
  emptyFunc206();
  emptyFunc207();
  emptyFunc208();
  emptyFunc209();
  emptyFunc210();
  emptyFunc211();
  emptyFunc212();
  emptyFunc213();
  emptyFunc214();
  emptyFunc215();
  emptyFunc216();
  emptyFunc217();
  emptyFunc218();
  emptyFunc219();
  emptyFunc220();
  emptyFunc221();
  emptyFunc222();
  emptyFunc223();
  emptyFunc224();
  emptyFunc225();
  emptyFunc226();
  emptyFunc227();
  emptyFunc228();
  emptyFunc229();
  emptyFunc230();
  emptyFunc231();
  emptyFunc232();
  emptyFunc233();
  emptyFunc234();
  emptyFunc235();
  emptyFunc236();
  emptyFunc237();
  emptyFunc238();
  emptyFunc239();
  emptyFunc240();
  emptyFunc241();
  emptyFunc242();
  emptyFunc243();
  emptyFunc244();
  emptyFunc245();
  emptyFunc246();
  emptyFunc247();
  emptyFunc248();
  emptyFunc249();
  emptyFunc250();
  emptyFunc251();
  emptyFunc252();
  emptyFunc253();
  emptyFunc254();
  emptyFunc255();
  emptyFunc256();
  emptyFunc257();
  emptyFunc258();
  emptyFunc259();
  emptyFunc260();
  emptyFunc261();
  emptyFunc262();
  emptyFunc263();
  emptyFunc264();
  emptyFunc265();
  emptyFunc266();
  emptyFunc267();
  emptyFunc268();
  emptyFunc269();
  emptyFunc270();
  emptyFunc271();
  emptyFunc272();
  emptyFunc273();
  emptyFunc274();
  emptyFunc275();
  emptyFunc276();
  emptyFunc277();
  emptyFunc278();
  emptyFunc279();
  emptyFunc280();
  emptyFunc281();
  emptyFunc282();
  emptyFunc283();
  emptyFunc284();
  emptyFunc285();
  emptyFunc286();
  emptyFunc287();
  emptyFunc288();
  emptyFunc289();
  emptyFunc290();
  emptyFunc291();
  emptyFunc292();
  emptyFunc293();
  emptyFunc294();
  emptyFunc295();
  emptyFunc296();
  emptyFunc297();
  emptyFunc298();
  emptyFunc299();
  emptyFunc300();
  emptyFunc301();
  emptyFunc302();
  emptyFunc303();
  emptyFunc304();
  emptyFunc305();
  emptyFunc306();
  emptyFunc307();
  emptyFunc308();
  emptyFunc309();
  emptyFunc310();
  emptyFunc311();
  emptyFunc312();
  emptyFunc313();
  emptyFunc314();
  emptyFunc315();
  emptyFunc316();
  emptyFunc317();
  emptyFunc318();
  emptyFunc319();
  emptyFunc320();
  emptyFunc321();
  emptyFunc322();
  emptyFunc323();
  emptyFunc324();
  emptyFunc325();
  emptyFunc326();
  emptyFunc327();
  emptyFunc328();
  emptyFunc329();
  emptyFunc330();
  emptyFunc331();
  emptyFunc332();
  emptyFunc333();
  emptyFunc334();
  emptyFunc335();
  emptyFunc336();
  emptyFunc337();
  emptyFunc338();
  emptyFunc339();
  emptyFunc340();
  emptyFunc341();
  emptyFunc342();
  emptyFunc343();
  emptyFunc344();
  emptyFunc345();
  emptyFunc346();
  emptyFunc347();
  emptyFunc348();
  emptyFunc349();
  emptyFunc350();
  emptyFunc351();
  emptyFunc352();
  emptyFunc353();
  emptyFunc354();
  emptyFunc355();
  emptyFunc356();
  emptyFunc357();
  emptyFunc358();
  emptyFunc359();
  emptyFunc360();
  emptyFunc361();
  emptyFunc362();
  emptyFunc363();
  emptyFunc364();
  emptyFunc365();
  emptyFunc366();
  emptyFunc367();
  emptyFunc368();
  emptyFunc369();
  emptyFunc370();
  emptyFunc371();
  emptyFunc372();
  emptyFunc373();
  emptyFunc374();
  emptyFunc375();
  emptyFunc376();
  emptyFunc377();
  emptyFunc378();
  emptyFunc379();
  emptyFunc380();
  emptyFunc381();
  emptyFunc382();
  emptyFunc383();
  emptyFunc384();
  emptyFunc385();
  emptyFunc386();
  emptyFunc387();
  emptyFunc388();
  emptyFunc389();
  emptyFunc390();
  emptyFunc391();
  emptyFunc392();
  emptyFunc393();
  emptyFunc394();
  emptyFunc395();
  emptyFunc396();
  emptyFunc397();
  emptyFunc398();
  emptyFunc399();
  emptyFunc400();
  emptyFunc401();
  emptyFunc402();
  emptyFunc403();
  emptyFunc404();
  emptyFunc405();
  emptyFunc406();
  emptyFunc407();
  emptyFunc408();
  emptyFunc409();
  emptyFunc410();
  emptyFunc411();
  emptyFunc412();
  emptyFunc413();
  emptyFunc414();
  emptyFunc415();
  emptyFunc416();
  emptyFunc417();
  emptyFunc418();
  emptyFunc419();
  emptyFunc420();
  emptyFunc421();
  emptyFunc422();
  emptyFunc423();
  emptyFunc424();
  emptyFunc425();
  emptyFunc426();
  emptyFunc427();
  emptyFunc428();
  emptyFunc429();
  emptyFunc430();
  emptyFunc431();
  emptyFunc432();
  emptyFunc433();
  emptyFunc434();
  emptyFunc435();
  emptyFunc436();
  emptyFunc437();
  emptyFunc438();
  emptyFunc439();
  emptyFunc440();
  emptyFunc441();
  emptyFunc442();
  emptyFunc443();
  emptyFunc444();
  emptyFunc445();
  emptyFunc446();
  emptyFunc447();
  emptyFunc448();
  emptyFunc449();
  emptyFunc450();
  emptyFunc451();
  emptyFunc452();
  emptyFunc453();
  emptyFunc454();
  emptyFunc455();
  emptyFunc456();
  emptyFunc457();
  emptyFunc458();
  emptyFunc459();
  emptyFunc460();
  emptyFunc461();
  emptyFunc462();
  emptyFunc463();
  emptyFunc464();
  emptyFunc465();
  emptyFunc466();
  emptyFunc467();
  emptyFunc468();
  emptyFunc469();
  emptyFunc470();
  emptyFunc471();
  emptyFunc472();
  emptyFunc473();
  emptyFunc474();
  emptyFunc475();
  emptyFunc476();
  emptyFunc477();
  emptyFunc478();
  emptyFunc479();
  emptyFunc480();
  emptyFunc481();
  emptyFunc482();
  emptyFunc483();
  emptyFunc484();
  emptyFunc485();
  emptyFunc486();
  emptyFunc487();
  emptyFunc488();
  emptyFunc489();
  emptyFunc490();
  emptyFunc491();
  emptyFunc492();
  emptyFunc493();
  emptyFunc494();
  emptyFunc495();
  emptyFunc496();
  emptyFunc497();
  emptyFunc498();
  emptyFunc499();
  emptyFunc500();
  emptyFunc501();
  emptyFunc502();
  emptyFunc503();
  emptyFunc504();
  emptyFunc505();
  emptyFunc506();
  emptyFunc507();
  emptyFunc508();
  emptyFunc509();
  emptyFunc510();
  emptyFunc511();
  emptyFunc512();
  emptyFunc513();
  emptyFunc514();
  emptyFunc515();
  emptyFunc516();
  emptyFunc517();
  emptyFunc518();
  emptyFunc519();
  emptyFunc520();
  emptyFunc521();
  emptyFunc522();
  emptyFunc523();
  emptyFunc524();
  emptyFunc525();
  emptyFunc526();
  emptyFunc527();
  emptyFunc528();
  emptyFunc529();
  emptyFunc530();
  emptyFunc531();
  emptyFunc532();
  emptyFunc533();
  emptyFunc534();
  emptyFunc535();
  emptyFunc536();
  emptyFunc537();
  emptyFunc538();
  emptyFunc539();
  emptyFunc540();
  emptyFunc541();
  emptyFunc542();
  emptyFunc543();
  emptyFunc544();
  emptyFunc545();
  emptyFunc546();
  emptyFunc547();
  emptyFunc548();
  emptyFunc549();
  emptyFunc550();
  emptyFunc551();
  emptyFunc552();
  emptyFunc553();
  emptyFunc554();
  emptyFunc555();
  emptyFunc556();
  emptyFunc557();
  emptyFunc558();
  emptyFunc559();
  emptyFunc560();
  emptyFunc561();
  emptyFunc562();
  emptyFunc563();
  emptyFunc564();
  emptyFunc565();
  emptyFunc566();
  emptyFunc567();
  emptyFunc568();
  emptyFunc569();
  emptyFunc570();
  emptyFunc571();
  emptyFunc572();
  emptyFunc573();
  emptyFunc574();
  emptyFunc575();
  emptyFunc576();
  emptyFunc577();
  emptyFunc578();
  emptyFunc579();
  emptyFunc580();
  emptyFunc581();
  emptyFunc582();
  emptyFunc583();
  emptyFunc584();
  emptyFunc585();
  emptyFunc586();
  emptyFunc587();
  emptyFunc588();
  emptyFunc589();
  emptyFunc590();
  emptyFunc591();
  emptyFunc592();
  emptyFunc593();
  emptyFunc594();
  emptyFunc595();
  emptyFunc596();
  emptyFunc597();
  emptyFunc598();
  emptyFunc599();
  emptyFunc600();
  emptyFunc601();
  emptyFunc602();
  emptyFunc603();
  emptyFunc604();
  emptyFunc605();
  emptyFunc606();
  emptyFunc607();
  emptyFunc608();
  emptyFunc609();
  emptyFunc610();
  emptyFunc611();
  emptyFunc612();
  emptyFunc613();
  emptyFunc614();
  emptyFunc615();
  emptyFunc616();
  emptyFunc617();
  emptyFunc618();
  emptyFunc619();
  emptyFunc620();
  emptyFunc621();
  emptyFunc622();
  emptyFunc623();
  emptyFunc624();
  emptyFunc625();
  emptyFunc626();
  emptyFunc627();
  emptyFunc628();
  emptyFunc629();
  emptyFunc630();
  emptyFunc631();
  emptyFunc632();
  emptyFunc633();
  emptyFunc634();
  emptyFunc635();
  emptyFunc636();
  emptyFunc637();
  emptyFunc638();
  emptyFunc639();
  emptyFunc640();
  emptyFunc641();
  emptyFunc642();
  emptyFunc643();
  emptyFunc644();
  emptyFunc645();
  emptyFunc646();
  emptyFunc647();
  emptyFunc648();
  emptyFunc649();
  emptyFunc650();
  emptyFunc651();
  emptyFunc652();
  emptyFunc653();
  emptyFunc654();
  emptyFunc655();
  emptyFunc656();
  emptyFunc657();
  emptyFunc658();
  emptyFunc659();
  emptyFunc660();
  emptyFunc661();
  emptyFunc662();
  emptyFunc663();
  emptyFunc664();
  emptyFunc665();
  emptyFunc666();
  emptyFunc667();
  emptyFunc668();
  emptyFunc669();
  emptyFunc670();
  emptyFunc671();
  emptyFunc672();
  emptyFunc673();
  emptyFunc674();
  emptyFunc675();
  emptyFunc676();
  emptyFunc677();
  emptyFunc678();
  emptyFunc679();
  emptyFunc680();
  emptyFunc681();
  emptyFunc682();
  emptyFunc683();
  emptyFunc684();
  emptyFunc685();
  emptyFunc686();
  emptyFunc687();
  emptyFunc688();
  emptyFunc689();
  emptyFunc690();
  emptyFunc691();
  emptyFunc692();
  emptyFunc693();
  emptyFunc694();
  emptyFunc695();
  emptyFunc696();
  emptyFunc697();
  emptyFunc698();
  emptyFunc699();
  emptyFunc700();
  emptyFunc701();
  emptyFunc702();
  emptyFunc703();
  emptyFunc704();
  emptyFunc705();
  emptyFunc706();
  emptyFunc707();
  emptyFunc708();
  emptyFunc709();
  emptyFunc710();
  emptyFunc711();
  emptyFunc712();
  emptyFunc713();
  emptyFunc714();
  emptyFunc715();
  emptyFunc716();
  emptyFunc717();
  emptyFunc718();
  emptyFunc719();
  emptyFunc720();
  emptyFunc721();
  emptyFunc722();
  emptyFunc723();
  emptyFunc724();
  emptyFunc725();
  emptyFunc726();
  emptyFunc727();
  emptyFunc728();
  emptyFunc729();
  emptyFunc730();
  emptyFunc731();
  emptyFunc732();
  emptyFunc733();
  emptyFunc734();
  emptyFunc735();
  emptyFunc736();
  emptyFunc737();
  emptyFunc738();
  emptyFunc739();
  emptyFunc740();
  emptyFunc741();
  emptyFunc742();
  emptyFunc743();
  emptyFunc744();
  emptyFunc745();
  emptyFunc746();
  emptyFunc747();
  emptyFunc748();
  emptyFunc749();
  emptyFunc750();
  emptyFunc751();
  emptyFunc752();
  emptyFunc753();
  emptyFunc754();
  emptyFunc755();
  emptyFunc756();
  emptyFunc757();
  emptyFunc758();
  emptyFunc759();
  emptyFunc760();
  emptyFunc761();
  emptyFunc762();
  emptyFunc763();
  emptyFunc764();
  emptyFunc765();
  emptyFunc766();
  emptyFunc767();
  emptyFunc768();
  emptyFunc769();
  emptyFunc770();
  emptyFunc771();
  emptyFunc772();
  emptyFunc773();
  emptyFunc774();
  emptyFunc775();
  emptyFunc776();
  emptyFunc777();
  emptyFunc778();
  emptyFunc779();
  emptyFunc780();
  emptyFunc781();
  emptyFunc782();
  emptyFunc783();
  emptyFunc784();
  emptyFunc785();
  emptyFunc786();
  emptyFunc787();
  emptyFunc788();
  emptyFunc789();
  emptyFunc790();
  emptyFunc791();
  emptyFunc792();
  emptyFunc793();
  emptyFunc794();
  emptyFunc795();
  emptyFunc796();
  emptyFunc797();
  emptyFunc798();
  emptyFunc799();
  emptyFunc800();
  emptyFunc801();
  emptyFunc802();
  emptyFunc803();
  emptyFunc804();
  emptyFunc805();
  emptyFunc806();
  emptyFunc807();
  emptyFunc808();
  emptyFunc809();
  emptyFunc810();
  emptyFunc811();
  emptyFunc812();
  emptyFunc813();
  emptyFunc814();
  emptyFunc815();
  emptyFunc816();
  emptyFunc817();
  emptyFunc818();
  emptyFunc819();
  emptyFunc820();
  emptyFunc821();
  emptyFunc822();
  emptyFunc823();
  emptyFunc824();
  emptyFunc825();
  emptyFunc826();
  emptyFunc827();
  emptyFunc828();
  emptyFunc829();
  emptyFunc830();
  emptyFunc831();
  emptyFunc832();
  emptyFunc833();
  emptyFunc834();
  emptyFunc835();
  emptyFunc836();
  emptyFunc837();
  emptyFunc838();
  emptyFunc839();
  emptyFunc840();
  emptyFunc841();
  emptyFunc842();
  emptyFunc843();
  emptyFunc844();
  emptyFunc845();
  emptyFunc846();
  emptyFunc847();
  emptyFunc848();
  emptyFunc849();
  emptyFunc850();
  emptyFunc851();
  emptyFunc852();
  emptyFunc853();
  emptyFunc854();
  emptyFunc855();
  emptyFunc856();
  emptyFunc857();
  emptyFunc858();
  emptyFunc859();
  emptyFunc860();
  emptyFunc861();
  emptyFunc862();
  emptyFunc863();
  emptyFunc864();
  emptyFunc865();
  emptyFunc866();
  emptyFunc867();
  emptyFunc868();
  emptyFunc869();
  emptyFunc870();
  emptyFunc871();
  emptyFunc872();
  emptyFunc873();
  emptyFunc874();
  emptyFunc875();
  emptyFunc876();
  emptyFunc877();
  emptyFunc878();
  emptyFunc879();
  emptyFunc880();
  emptyFunc881();
  emptyFunc882();
  emptyFunc883();
  emptyFunc884();
  emptyFunc885();
  emptyFunc886();
  emptyFunc887();
  emptyFunc888();
  emptyFunc889();
  emptyFunc890();
  emptyFunc891();
  emptyFunc892();
  emptyFunc893();
  emptyFunc894();
  emptyFunc895();
  emptyFunc896();
  emptyFunc897();
  emptyFunc898();
  emptyFunc899();
  emptyFunc900();
  emptyFunc901();
  emptyFunc902();
  emptyFunc903();
  emptyFunc904();
  emptyFunc905();
  emptyFunc906();
  emptyFunc907();
  emptyFunc908();
  emptyFunc909();
  emptyFunc910();
  emptyFunc911();
  emptyFunc912();
  emptyFunc913();
  emptyFunc914();
  emptyFunc915();
  emptyFunc916();
  emptyFunc917();
  emptyFunc918();
  emptyFunc919();
  emptyFunc920();
  emptyFunc921();
  emptyFunc922();
  emptyFunc923();
  emptyFunc924();
  emptyFunc925();
  emptyFunc926();
  emptyFunc927();
  emptyFunc928();
  emptyFunc929();
  emptyFunc930();
  emptyFunc931();
  emptyFunc932();
  emptyFunc933();
  emptyFunc934();
  emptyFunc935();
  emptyFunc936();
  emptyFunc937();
  emptyFunc938();
  emptyFunc939();
  emptyFunc940();
  emptyFunc941();
  emptyFunc942();
  emptyFunc943();
  emptyFunc944();
  emptyFunc945();
  emptyFunc946();
  emptyFunc947();
  emptyFunc948();
  emptyFunc949();
  emptyFunc950();
  emptyFunc951();
  emptyFunc952();
  emptyFunc953();
  emptyFunc954();
  emptyFunc955();
  emptyFunc956();
  emptyFunc957();
  emptyFunc958();
  emptyFunc959();
  emptyFunc960();
  emptyFunc961();
  emptyFunc962();
  emptyFunc963();
  emptyFunc964();
  emptyFunc965();
  emptyFunc966();
  emptyFunc967();
  emptyFunc968();
  emptyFunc969();
  emptyFunc970();
  emptyFunc971();
  emptyFunc972();
  emptyFunc973();
  emptyFunc974();
  emptyFunc975();
  emptyFunc976();
  emptyFunc977();
  emptyFunc978();
  emptyFunc979();
  emptyFunc980();
  emptyFunc981();
  emptyFunc982();
  emptyFunc983();
  emptyFunc984();
  emptyFunc985();
  emptyFunc986();
  emptyFunc987();
  emptyFunc988();
  emptyFunc989();
  emptyFunc990();
  emptyFunc991();
  emptyFunc992();
  emptyFunc993();
  emptyFunc994();
  emptyFunc995();
  emptyFunc996();
  emptyFunc997();
  emptyFunc998();
  emptyFunc999();
  emptyFunc1000();
  emptyFunc1001();
  emptyFunc1002();
  emptyFunc1003();
  emptyFunc1004();
  emptyFunc1005();
  emptyFunc1006();
  emptyFunc1007();
  emptyFunc1008();
  emptyFunc1009();
  emptyFunc1010();
  emptyFunc1011();
  emptyFunc1012();
  emptyFunc1013();
  emptyFunc1014();
  emptyFunc1015();
  emptyFunc1016();
  emptyFunc1017();
  emptyFunc1018();
  emptyFunc1019();
  emptyFunc1020();
  emptyFunc1021();
  emptyFunc1022();
  emptyFunc1023();

  deactivation_costs = new ticks[2*max_probe_id]();
  activation_costs   = new ticks[2*max_probe_id]();

  ticks start, end;  
  ticks deactivation_sum = 0;
  ticks activation_sum = 0;

  FILE* fp1 = fopen("toggle_cost_deactivation.csv", "w");
  FILE* fp2 = fopen("toggle_cost_activation.csv", "w");

  int i = 0; 
  
  for (const auto& it : pr.pg_by_function) { 

    for (ProbeGroupInfo pgi : it.second) {
      start = getticks();
      p->deactivate(pgi); 
      end = getticks();

      deactivation_costs[i] = (end - start);

      // Filtering out outliers
      if (deactivation_costs[i] < OUTLIER) {
        deactivation_sum += deactivation_costs[i];
        fprintf(fp1, "%d, %llu\n", i, deactivation_costs[i]);
      }
      i++;
    }
  }
 
  i = 0; 
  for (const auto& it : pr.pg_by_function) {
    for (ProbeGroupInfo pgi : it.second) { 
      start = getticks();
      p->activate(pgi);
      end = getticks();

      activation_costs[i] = (end - start);
    
      // Filtering out outliers
      if (activation_costs[i] < OUTLIER) {
        activation_sum += activation_costs[i];
        fprintf(fp2, "%d, %llu\n", i, activation_costs[i]);
      }
      i++;
    }
  }

  fprintf(stderr, "Number of functions : %ld\n", NUM_FUNCS);

// multiply by 2 because of activating/deactivating two probes per fun
  fprintf(stderr, "Probe deactivation cost estimate (cycles) : %llu\n", 
	  deactivation_sum / (2 * max_probe_id));

  fprintf(stderr, "Probe activation cost estimate (cycles) : %llu\n", 
	  activation_sum / (2 * max_probe_id));
 
  fclose(fp1);
  fclose(fp2);

  delete[] deactivation_costs;
  delete[] activation_costs;

  exit(EXIT_SUCCESS);

}
