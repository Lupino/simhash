#ifndef SimHash_H_
#define SimHash_H_
#include <htm/algorithms/SpatialPooler.hpp>
#include <htm/algorithms/SDRClassifier.hpp>
#include <htm/encoders/SimHashDocumentEncoder.hpp>
#include <htm/types/Serializable.hpp>

namespace simhash {

using namespace std;
using namespace htm;

class SimHash : public Serializable {
  private:
    SpatialPooler sp;
    SimHashDocumentEncoder encoder;
    Classifier clsr;
    SDR encoderOutput;
    SDR spOutput;

  public:
    SimHash();
    void setup();
    void learn(const std::string input, const UInt categoryIdx);
    void infer(const std::string input, double* out);
    void loadFromFileV2(const std::string spFile, const std::string clsrFile);

    CerealAdapter;
    template<class Archive>
    void save_ar(Archive & ar) const
    {
      ar(cereal::make_nvp("sp", sp),
         cereal::make_nvp("clsr", clsr));
    }

    template<class Archive>
    void load_ar(Archive & ar) {
      ar(cereal::make_nvp("sp", sp),
         cereal::make_nvp("clsr", clsr));
    }
};  // End class SimHash

} // -ns

#endif  // SimHash_H_
