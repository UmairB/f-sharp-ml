namespace MachineLearning.DigitRecognizer.CSharp
{
    public class Observation
    {
        public string Label { get; }

        public int[] Pixels { get; }

        public Observation(string label, int[] pixels)
        {
            this.Label = label;
            this.Pixels = pixels;
        }
    }
}
