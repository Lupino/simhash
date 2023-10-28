#include "sdr.h"
#include <htm/types/Sdr.hpp>
#include <htm/algorithms/SDRClassifier.hpp>
#include <htm/encoders/SimHashDocumentEncoder.hpp>
#include <htm/algorithms/SpatialPooler.hpp>

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

extern void cClassifierSaveToFile(char * bs, int len, void * classifier) {
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    std::string fn(bs);
    fn.resize(len);
    _classifier -> saveToFile(fn);
}

extern void cClassifierLoadFromFile(char * bs, int len, void * classifier) {
    htm::Classifier * _classifier = (htm::Classifier *)classifier;
    std::string fn(bs);
    fn.resize(len);
    _classifier -> loadFromFile(fn);
}

extern void cSpatialPoolerSaveToFile(char * bs, int len, void * pooler) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    std::string fn(bs);
    fn.resize(len);
    _pooler -> saveToFile(fn);
}

extern void cSpatialPoolerLoadFromFile(char * bs, int len, void * pooler) {
    htm::SpatialPooler * _pooler = (htm::SpatialPooler *)pooler;
    std::string fn(bs);
    fn.resize(len);
    _pooler -> loadFromFile(fn);
}
