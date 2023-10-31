#ifndef SDR_H_
#define SDR_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void * new_sdr();
extern void delete_sdr(void * sdr);
extern void sdr_initialize(int * dims, int dim_len, void * sdr);

extern void * new_classifier();
extern void delete_classifier(void * clsr);
extern void classifier_learn(void * sdr, int idx, void * clsr);
extern void classifier_infer(void * sdr, double* out, void * clsr);
extern void classifier_initialize(double alpha, void * clsr);

extern void * new_simHashDocumentEncoder();
extern void delete_simHashDocumentEncoder(void * encoder);
extern void simHashDocumentEncoder_initialize(int size, double sparsity, bool tokenSimilarity, void * encoder);
extern void simHashDocumentEncoder_encode(char * bs, int len, void * sdr, void * encoder);

extern void * new_spatialPooler();
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
    void * pooler);
extern void delete_spatialPooler(void * pooler);
extern void spatialPooler_compute(void * sdr, bool learn, void * active, void * pooler);

extern void saveToFile(char * bs, int len, void * pooler, void * classifier, void * encoder, void * sdr0, void * sdr1, char *labels, int label_len);
extern void loadFromFile(char * bs, int len, void * pooler, void * classifier, void * encoder, void * sdr0, void * sdr1, char *labels, int *label_len);

#ifdef __cplusplus
}
#endif

#endif  // SDR_H_
