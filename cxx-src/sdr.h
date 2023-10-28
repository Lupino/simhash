#ifndef SDR_H_
#define SDR_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void * newCSdr(int dim);
extern void deleteCSdr(void * sdr);

extern void * newCClassifier();
extern void deleteCClassifier(void * classifier);
extern void cClassifierLearn(void * sdr, int cls, void * classifier);
extern void cClassifierInfer(void * sdr, double* ret, void * classifier);


extern void * newCSimHashDocumentEncoder(int size, double sparsity, bool tokenSimilarity);
extern void deleteCSimHashDocumentEncoder(void * encoder);
extern void cSimHashDocumentEncoderLearn(char * bs, int len, void * sdr, void * encoder);

extern void * newCSpatialPooler(int inputDim, int columnDim);
extern void deleteCSpatialPooler(void * pooler);
extern void cSpatialPoolerCompute(void * sdr, bool learn, void * active, void * pooler);


extern void cClassifierSaveToFile(char * bs, int len, void * classifier);
extern void cClassifierLoadFromFile(char * bs, int len, void * classifier);


extern void cSpatialPoolerSaveToFile(char * bs, int len, void * pooler);
extern void cSpatialPoolerLoadFromFile(char * bs, int len, void * pooler);


#ifdef __cplusplus
}
#endif

#endif  // SDR_H_
