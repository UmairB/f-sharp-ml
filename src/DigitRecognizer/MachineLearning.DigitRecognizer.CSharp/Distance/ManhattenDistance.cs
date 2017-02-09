using System;

namespace MachineLearning.DigitRecognizer.CSharp.Distance
{
    public class ManhattenDistance : IDistance
    {
        public double Between(int[] pixelsOne, int[] pixelsTwo)
        {
            if (pixelsOne.Length != pixelsTwo.Length)
            {
                throw new ArgumentException("Inconsistent image sizes.");
            }

            var length = pixelsOne.Length;

            var distance = 0;

            for (var i = 0; i < length; i++)
            {
                distance += Math.Abs(pixelsOne[i] - pixelsTwo[i]);
            }

            return distance;
        }
    }
}
