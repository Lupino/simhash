{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "simhash"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Li Meng Jun";
      homepage = "https://github.com/Lupino/simhash#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
          (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."inline-c" or (errorHandler.buildDepError "inline-c"))
          (hsPkgs."inline-c-cpp" or (errorHandler.buildDepError "inline-c-cpp"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
        libs = (pkgs.lib).optional (system.isOsx) (pkgs."boost_filesystem" or (errorHandler.sysDepError "boost_filesystem"));
        buildable = true;
        modules = [ "SimHash" ];
        cxxSources = [
          "cxx-src/htm.core/src/htm/types/Sdr.cpp"
          "cxx-src/htm.core/src/htm/algorithms/Connections.cpp"
          "cxx-src/htm.core/src/htm/algorithms/SDRClassifier.cpp"
          "cxx-src/htm.core/src/htm/algorithms/SpatialPooler.cpp"
          "cxx-src/htm.core/src/htm/utils/Random.cpp"
          "cxx-src/htm.core/src/htm/utils/Topology.cpp"
          "cxx-src/htm.core/src/htm/utils/SdrMetrics.cpp"
          "cxx-src/htm.core/src/htm/encoders/SimHashDocumentEncoder.cpp"
          "cxx-src/htm.core/src/htm/os/Directory.cpp"
          "cxx-src/htm.core/src/htm/os/Path.cpp"
          "cxx-src/SimHash.cpp"
          "cxx-src/MyLog.cpp"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [
          "cxx-src"
          "cxx-src/digestpp"
          "cxx-src/eigen"
          "cxx-src/cereal/include"
          "cxx-src/htm.core/src"
          "cxx-src/htm.core/external/common"
          ];
        };
      exes = {
        "simhash-train" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."simhash" or (errorHandler.buildDepError "simhash"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "simhash-train.hs" ];
          };
        "simhash-infer" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."simhash" or (errorHandler.buildDepError "simhash"))
            (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "simhash-infer.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }