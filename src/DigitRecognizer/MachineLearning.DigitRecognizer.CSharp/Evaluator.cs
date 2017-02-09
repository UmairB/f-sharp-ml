using MachineLearning.DigitRecognizer.CSharp.Classifier;
using System.Collections.Generic;
using System.Linq;

namespace MachineLearning.DigitRecognizer.CSharp
{
    public class Evaluator
    {
        public static double Correct(IEnumerable<Observation> validationSet, IClassifier classifier)
        {
            return validationSet
                .Select(obs => Score(obs, classifier))
                .Average();
        }

        private static double Score(Observation obs, IClassifier classifier)
        {
            return classifier.Predict(obs.Pixels) == obs.Label ? 1.0 : 0.0;
        }
    }
}
