#include "sdr.h"
#include <htm/types/Sdr.hpp>
#include <htm/algorithms/SDRClassifier.hpp>
#include <htm/encoders/SimHashDocumentEncoder.hpp>
#include <htm/algorithms/SpatialPooler.hpp>
#include <htm/types/Serializable.hpp>

extern void * newCSdr(int dim) {
   return new htm::SDR({(htm::UInt)dim});
}

extern void deleteCSdr(void * sdr) {
   htm::SDR * _sdr = (htm::SDR *)sdr;
   delete _sdr;
}

extern void * newCClassifier() {
    return new htm::Classifier();
}
extern void deleteCClassifier(void * classifier) {
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    delete _classifier;
}
extern void cClassifierLearn(void * sdr, int cls, void * classifier) {
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    _classifier->learn(*(htm::SDR*)sdr, cls);
}
extern void cClassifierInfer(void * sdr, double* out, void * classifier) {
    htm::Classifier * _classifier = (htm::Classifier *)classifier;

    htm::PDF ret = _classifier->infer(*(htm::SDR*)sdr);

    for (int i=0;i<ret.size();i++) {
        out[i] = ret[i];
    }
}

extern void * newCSimHashDocumentEncoder(int size, double sparsity, bool tokenSimilarity) {
    htm::SimHashDocumentEncoderParameters params;
    params.size = size;
    params.sparsity = sparsity;
    params.tokenSimilarity = tokenSimilarity;
    return new htm::SimHashDocumentEncoder(params);
}

extern void deleteCSimHashDocumentEncoder(void * encoder) {
    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;
    delete _encoder;
}

extern void cSimHashDocumentEncoderEncode(char * bs, int len, void * sdr, void * encoder) {
    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;
    std::string str(bs);
    str.resize(len);
    _encoder->encode(str, *(htm::SDR*) sdr);
}

extern void * newCSpatialPooler(int inputDim, int columnDim) {
    return new htm::SpatialPooler({(htm::UInt)inputDim}, {(htm::UInt)columnDim});
}

extern void deleteCSpatialPooler(void * pooler) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    delete _pooler;
}

extern void cSpatialPoolerCompute(void * sdr, bool learn, void * active, void * pooler) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    _pooler->compute(*(const htm::SDR *)sdr, learn, *(htm::SDR*) active);
}

namespace saver {

using namespace std;
using namespace htm;

class Saver : public Serializable {
  private:
    SpatialPooler * sp;
    Classifier * clsr;
    string * labels;

  public:
    Saver(SpatialPooler * sp0, Classifier * clsr0, string *labels0) {
      sp = sp0;
      clsr = clsr0;
      labels = labels0;
    }
    CerealAdapter;
    template<class Archive>
    void save_ar(Archive & ar) const {
      ar(cereal::make_nvp("sp", * sp));
      ar(cereal::make_nvp("clsr", * clsr));
      ar(cereal::make_nvp("labels", * labels));
    }

    template<class Archive>
    void load_ar(Archive & ar) {
      ar(cereal::make_nvp("sp", * sp));
      ar(cereal::make_nvp("clsr", * clsr));
      ar(cereal::make_nvp("labels", * labels));
    }

};  // End class Saver

} // -ns

extern void saveToFile(char * bs, int len, void * pooler, void * classifier, char *labels, int label_len) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    std::string str_labels(labels);
    str_labels.resize(label_len);
    saver::Saver saver(_pooler, _classifier, &str_labels);
    std::string fn(bs);
    fn.resize(len);
    saver.saveToFile(fn);
}

extern void loadFromFile(char * bs, int len, void * pooler, void * classifier, char *labels, int* label_len) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    std::string str_labels;
    saver::Saver saver(_pooler, _classifier, &str_labels);
    std::string fn(bs);
    fn.resize(len);
    saver.loadFromFile(fn);

    memcpy(labels, str_labels.c_str(), str_labels.size());
    *label_len = str_labels.size();
}
