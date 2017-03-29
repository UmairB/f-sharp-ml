namespace MachineLearning.DigitRecognizer2

open System.Windows.Forms;
open System.Drawing;

module Render =
    let drawDigit (pixels:float[]) label =
        let tile = 20
        let form = new Form(TopMost = true,
                        Visible = true,
                        Width = 29 * tile,
                        Height = 29 * tile)
        
        let panel = new Panel(Dock = DockStyle.Fill)
        panel.BackColor <- Color.Black

        form.Controls.Add(panel)

        let graphics = panel.CreateGraphics()
        pixels
        |> Array.iteri (fun i p ->
            let col = i % 28
            let row = i % 28
            let color = Color.FromArgb(int p, int p, int p)
            let brush = new SolidBrush(color)

            graphics.FillRectangle(brush, col * tile, row * tile, tile, tile))

        let point = PointF((float32)5, (float32)5)
        let font = new Font(family = FontFamily.GenericSansSerif, emSize = (float32)30)

        graphics.DrawString(label, font, new SolidBrush(Color.YellowGreen), point)

        form.Show()
