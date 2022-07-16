#include "SimHash.hpp"

namespace simhash {

using namespace std;
using namespace htm;

SimHash::SimHash():
  encoderOutputStats({600}, 1024),
  spOutputStats({1600}, 1024)
{
  SimHashDocumentEncoderParameters encoderParameters;
  encoderParameters.size = 600;
  encoderParameters.sparsity = 0.2;
  encoder.initialize(encoderParameters);
  encoderOutput.initialize({600});
  spOutput.initialize({1600});
}

void SimHash::setup() {
  sp.initialize(
    /* inputDimensions */             {600},
    /* columnDimensions */            {1600},
    /* potentialRadius */             16,
    /* potentialPct */                0.5f,
    /* globalInhibition */            true,
    /* localAreaDensity */            0.02f,  // % active bits
    /* numActiveColumnsPerInhArea */  0,
    /* stimulusThreshold */           6u,
    /* synPermInactiveDec */          0.01f,
    /* synPermActiveInc */            0.1f, //takes upto 5x steps to get dis/connected
    /* synPermConnected */            0.1f, //no difference, let's leave at 0.5 in the middle
    /* minPctOverlapDutyCycles */     0.001f, //speed of re-learning?
    /* dutyCyclePeriod */             1000,
    /* boostStrength */               0.0f,
    /* seed */                        0u,
    /* spVerbosity */                 99u,
    /* wrapAround */                  true); // does not matter (helps slightly)

  clsr.initialize( /* alpha */ 0.001f);
}


void SimHash::learn(const std::string input, const UInt categoryIdx) {
  encoder.encode(input, encoderOutput);
  sp.compute(encoderOutput, true, spOutput);
  clsr.learn(spOutput, categoryIdx);
}

void SimHash::infer(const std::string input, double* out) {
  encoder.encode(input, encoderOutput);
  sp.compute(encoderOutput, false, spOutput);
  PDF ret = clsr.infer(spOutput);

  for (int i=0;i<ret.size();i++) {
      out[i] = ret[i];
  }
}

void SimHash::addMetrics() {
  encoderOutputStats.addData(encoderOutput);
  spOutputStats.addData(spOutput);
}

void SimHash::showMetrics() {
  cout << "encoderStats " << encoderOutputStats << endl;
  cout << "spStats " << spOutputStats << endl;
}

}
