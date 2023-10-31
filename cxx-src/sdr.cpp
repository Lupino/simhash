#include "sdr.h"
#include <htm/types/Sdr.hpp>
#include <htm/algorithms/SDRClassifier.hpp>
#include <htm/encoders/SimHashDocumentEncoder.hpp>
#include <htm/algorithms/SpatialPooler.hpp>
#include <htm/types/Serializable.hpp>

extern void * new_sdr() {
    return new htm::SDR();
}

extern void delete_sdr(void * sdr) {
    htm::SDR * _sdr = (htm::SDR *)sdr;
    delete _sdr;
}

extern void sdr_initialize(int * dims, int dim_len, void * sdr) {
    std::vector<htm::UInt> v_dims;
    for (int i = 0; i < dim_len; i ++) {
      v_dims.push_back((htm::UInt) dims[i]);
    }

    htm::SDR * _sdr = (htm::SDR *)sdr;
    _sdr -> initialize(v_dims);
}

extern void * new_classifier() {
    return new htm::Classifier();
}

extern void delete_classifier(void * clsr) {
    htm::Classifier * _clsr = (htm::Classifier *)clsr;
    delete _clsr;
}

extern void classifier_learn(void * sdr, int idx, void * clsr) {
    htm::Classifier * _clsr = (htm::Classifier *)clsr;
    _clsr->learn(*(htm::SDR*)sdr, idx);
}

extern void classifier_infer(void * sdr, double* out, void * clsr) {
    htm::Classifier * _clsr = (htm::Classifier *)clsr;
    htm::PDF ret = _clsr->infer(*(htm::SDR*)sdr);
    for (int i=0;i<ret.size();i++) {
        out[i] = ret[i];
    }
}

extern void classifier_initialize(double alpha, void * clsr) {
    htm::Classifier * _clsr = (htm::Classifier *)clsr;
    _clsr->initialize((htm::Real)alpha);
}

extern void * new_simHashDocumentEncoder() {
    return new htm::SimHashDocumentEncoder();
}

extern void delete_simHashDocumentEncoder(void * encoder) {
    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;
    delete _encoder;
}

extern void simHashDocumentEncoder_initialize(int size, double sparsity, bool tokenSimilarity, void * encoder) {
    htm::SimHashDocumentEncoderParameters params;
    params.size = size;
    params.sparsity = sparsity;
    params.tokenSimilarity = tokenSimilarity;

    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;

    _encoder->initialize(params);
}

extern void simHashDocumentEncoder_encode(char * bs, int len, void * sdr, void * encoder) {
    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;
    std::string str(bs);
    str.resize(len);
    _encoder->encode(str, *(htm::SDR*) sdr);
}

extern void * new_spatialPooler() {
    return new htm::SpatialPooler();
}

extern void spatialPooler_initialize(
    int * inputDimensions_ptr, int input_len,
    int * columnDimensions_ptr, int column_len,
    int potentialRadius, float potentialPct,
    bool globalInhibition, float localAreaDensity,
    int numActiveColumnsPerInhArea, int stimulusThreshold,
    float synPermInactiveDec, float synPermActiveInc,
    float synPermConnected, float minPctOverlapDutyCycles,
    int dutyCyclePeriod, float boostStrength,
    int seed, int spVerbosity, bool wrapAround,
    void * pooler) {
    std::vector<htm::UInt> inputDimensions;
    for (int i = 0; i < input_len; i ++) {
      inputDimensions.push_back((htm::UInt) inputDimensions_ptr[i]);
    }
    std::vector<htm::UInt> columnDimensions;
    for (int i = 0; i < column_len; i ++) {
      columnDimensions.push_back((htm::UInt) columnDimensions_ptr[i]);
    }
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    _pooler->initialize(
        inputDimensions,
        columnDimensions,
        potentialRadius,
        potentialPct,
        globalInhibition,
        localAreaDensity,
        numActiveColumnsPerInhArea,
        stimulusThreshold,
        synPermInactiveDec, synPermActiveInc,
        synPermConnected, minPctOverlapDutyCycles,
        dutyCyclePeriod, boostStrength,
        seed, spVerbosity, wrapAround
    );
}

extern void delete_spatialPooler(void * pooler) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    delete _pooler;
}

extern void spatialPooler_compute(void * sdr, bool learn, void * active, void * pooler) {
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
    SimHashDocumentEncoder * encoder;
    SDR * sdr0;
    SDR * sdr1;
    string * labels;

  public:
    Saver(SpatialPooler * sp0, Classifier * clsr0, SimHashDocumentEncoder * encoder0, SDR * sdr00, SDR * sdr10, string *labels0) {
      sp = sp0;
      clsr = clsr0;
      encoder = encoder0;
      sdr0 = sdr00;
      sdr1 = sdr10;
      labels = labels0;
    }
    CerealAdapter;
    template<class Archive>
    void save_ar(Archive & ar) const {
      ar(cereal::make_nvp("sp", * sp));
      ar(cereal::make_nvp("clsr", * clsr));
      ar(cereal::make_nvp("encoder", * encoder));
      ar(cereal::make_nvp("sdr0", * sdr0));
      ar(cereal::make_nvp("sdr1", * sdr1));
      ar(cereal::make_nvp("labels", * labels));
    }

    template<class Archive>
    void load_ar(Archive & ar) {
      ar(cereal::make_nvp("sp", * sp));
      ar(cereal::make_nvp("clsr", * clsr));
      ar(cereal::make_nvp("encoder", * encoder));
      ar(cereal::make_nvp("sdr0", * sdr0));
      ar(cereal::make_nvp("sdr1", * sdr1));
      ar(cereal::make_nvp("labels", * labels));
    }

};  // End class Saver

} // -ns

extern void saveToFile(char * bs, int len, void * pooler, void * classifier, void * encoder, void * sdr0, void * sdr1, char *labels, int label_len) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;
    htm::SDR * _sdr0 = (htm::SDR *)sdr0;
    htm::SDR * _sdr1 = (htm::SDR *)sdr1;
    std::string str_labels(labels);
    str_labels.resize(label_len);
    saver::Saver saver(_pooler, _classifier, _encoder, _sdr0, _sdr1, &str_labels);
    std::string fn(bs);
    fn.resize(len);
    saver.saveToFile(fn);
}

extern void loadFromFile(char * bs, int len, void * pooler, void * classifier, void * encoder, void * sdr0, void * sdr1, char *labels, int* label_len) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    htm::SimHashDocumentEncoder * _encoder = (htm::SimHashDocumentEncoder *)encoder;
    htm::SDR * _sdr0 = (htm::SDR *)sdr0;
    htm::SDR * _sdr1 = (htm::SDR *)sdr1;
    std::string str_labels;
    saver::Saver saver(_pooler, _classifier, _encoder, _sdr0, _sdr1, &str_labels);
    std::string fn(bs);
    fn.resize(len);
    saver.loadFromFile(fn);

    memcpy(labels, str_labels.c_str(), str_labels.size());
    *label_len = str_labels.size();
}
