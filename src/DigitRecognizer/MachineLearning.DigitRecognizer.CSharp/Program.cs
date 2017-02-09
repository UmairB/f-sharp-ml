using MachineLearning.DigitRecognizer.CSharp.Classifier;
using MachineLearning.DigitRecognizer.CSharp.Distance;
using System;

namespace MachineLearning.DigitRecognizer.CSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            var manhattenDistance = new ManhattenDistance();
            var classifier = new BasicClassifier(manhattenDistance);

            var trainingDataPath = "../../../Data/trainingsample.csv";
            var training = DataReader.ReadObservations(trainingDataPath);
            var validationDataPath = "../../../Data/validationsample.csv";
            var validation = DataReader.ReadObservations(validationDataPath);

            classifier.Train(training);
            var correct = Evaluator.Correct(validation, classifier);

            Console.WriteLine("Manhatten: Correctly classified {0:P2}", correct);

            var euclideanDistance = new EuclideanDistance();
            classifier = new BasicClassifier(euclideanDistance);

            classifier.Train(training);
            correct = Evaluator.Correct(validation, classifier);

            Console.WriteLine("Euclidean: Correctly classified {0:P2}", correct);
            Console.ReadLine();
        }
    }
}
