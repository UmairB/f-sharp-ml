using System.Collections.Generic;

namespace MachineLearning.DigitRecognizer.CSharp.Classifier
{
    public interface IClassifier
    {
        void Train(IEnumerable<Observation> trainingSet);

        string Predict(int[] pixels);
    }
}
