#include <BPatch.h>
#include "BPatch_addressSpace.h"
#include "BPatch_process.h"
#include "BPatch_binaryEdit.h"
#include "BPatch_function.h"
#include "BPatch_point.h"
#include "BPatch_flowGraph.h"
#include <string>

BPatch bpatch;

int main()
{
  BPatch_addressSpace *app = bpatch.openBinary("test.exe");
  std::vector<BPatch_function *> function_insert_locations;
  std::vector<BPatch_function *> function_insertions;
  std::vector<BPatch_point *> *points;
  std::vector<BPatch_snippet*> args;
  
  BPatch_image *appImage = app->getImage();
  // Get inertion points for __notify_intrinsic
  appImage->findFunction("__notify_intrinsic", function_insert_locations);
  points = function_insert_locations[0]->findPoint(BPatch_entry);
  
  // Get testInsert function represented as a BPatch_function
  appImage->findFunction("testInsert", function_insertions);

  // ??? things
  BPatch_snippet *eae = new BPatch_effectiveAddressExpr();
  args.push_back(eae);
  BPatch_funcCallExpr testCall(*(function_insertions[0]), args);
  
  // Insert the snippet
  app-insertSnippet(testCall, *points);
  
  /*
  BPatch_variableExpr *intCounter = app->malloc(*(appImage->findType("int")));
  BPatch_arithExpr addOne(BPatch_assign, *intCounter, BPatch_arithExpr(BPatch_plus,
								       *intCounter,
								       BPatch_constExpr(1)));
  app->insertSnippet(addOne, *points);

  BPatch_binaryEdit *appBin = dynamic_cast<BPatch_binaryEdit *>(app);
  appBin->writeFile("new_test.exe");
  */
}
