namespace MachineLearning.DigitRecognizer.CSharp.Distance
{
    public interface IDistance
    {
        double Between(int[] pixelsOne, int[] pixelsTwo);
    }
}
