//
// simple.C
//
// a direct rip-off of James Clark's sample program
// for the generic SP parser.
//
// The next two lines are only to ensure bool gets defined
// appropriately.

#include <stdio.h>

#include "config.h"
#include "Boolean.h"

#include "ParserEventGeneratorKit.h"


class OutlineApplication : public SGMLApplication {
public:
  OutlineApplication() : depth_(0) { }

  void PrtStr (const CharString &s) {
    for (size_t i=0; i<s.len; i++) printf ("%c", s.ptr[2*i]);
    printf ("\n");
  }

  void data (const DataEvent &event) {
     PrtStr (event.data);
  }

  void startElement(const StartElementEvent &event) {
    for (unsigned i = 0; i < depth_; i++) printf ("    ");
    PrtStr (event.gi);
    depth_++;
  }

  void endElement(const EndElementEvent &) { depth_--; }

  void startDtd (const StartDtdEvent &event) {
    PrtStr (event.name);
  }

  void endDtd(const EndDtdEvent &event) {
    PrtStr (event.name);
  }

private:
  unsigned depth_;
};

int main(int argc, char **argv)
{
  ParserEventGeneratorKit parserKit;
  // Use all the arguments after argv[0] as filenames.
  EventGenerator *egp = parserKit.makeEventGenerator(argc - 1, argv + 1);
  OutlineApplication app;
  unsigned nErrors = egp->run(app);
  delete egp;
  return nErrors > 0;
}
