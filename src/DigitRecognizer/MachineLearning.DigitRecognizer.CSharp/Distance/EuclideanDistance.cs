using System;
using System.Linq;

namespace MachineLearning.DigitRecognizer.CSharp.Distance
{
    public class EuclideanDistance : IDistance
    {
        public double Between(int[] pixelsOne, int[] pixelsTwo)
        {
            if (pixelsOne.Length != pixelsTwo.Length)
            {
                throw new ArgumentException("Inconsistent image sizes.");
            }

            return pixelsOne
                .Zip(pixelsTwo, (x, y) => new[] { x, y })
                .Select(points => Math.Pow(points[0] - points[1], 2))
                .Sum();
        }
    }
}
