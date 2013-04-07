namespace VendingMachine.GUI
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.purchaseB = new System.Windows.Forms.Button();
            this.insertCoinB = new System.Windows.Forms.Button();
            this.coinsCB = new System.Windows.Forms.ComboBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.cancelB = new System.Windows.Forms.Button();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.daimATB = new System.Windows.Forms.TextBox();
            this.hariboATB = new System.Windows.Forms.TextBox();
            this.faxeATB = new System.Windows.Forms.TextBox();
            this.cokeATB = new System.Windows.Forms.TextBox();
            this.hariboB = new System.Windows.Forms.Button();
            this.faxeB = new System.Windows.Forms.Button();
            this.daimB = new System.Windows.Forms.Button();
            this.cokeB = new System.Windows.Forms.Button();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.dispL = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
            // 
            // purchaseB
            // 
            this.purchaseB.Cursor = System.Windows.Forms.Cursors.Default;
            this.purchaseB.Enabled = false;
            this.purchaseB.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.purchaseB.Location = new System.Drawing.Point(152, 19);
            this.purchaseB.Name = "purchaseB";
            this.purchaseB.Size = new System.Drawing.Size(106, 57);
            this.purchaseB.TabIndex = 0;
            this.purchaseB.Tag = "purchaseB";
            this.purchaseB.Text = "Purchase";
            this.purchaseB.UseVisualStyleBackColor = true;
            this.purchaseB.Click += new System.EventHandler(this.purchaseB_Click);
            // 
            // insertCoinB
            // 
            this.insertCoinB.Cursor = System.Windows.Forms.Cursors.Default;
            this.insertCoinB.Enabled = false;
            this.insertCoinB.Location = new System.Drawing.Point(152, 19);
            this.insertCoinB.Name = "insertCoinB";
            this.insertCoinB.Size = new System.Drawing.Size(106, 23);
            this.insertCoinB.TabIndex = 3;
            this.insertCoinB.Text = "Insert";
            this.insertCoinB.UseVisualStyleBackColor = true;
            this.insertCoinB.Click += new System.EventHandler(this.insertCoinB_Click);
            // 
            // coinsCB
            // 
            this.coinsCB.FormattingEnabled = true;
            this.coinsCB.Items.AddRange(new object[] {
            "Ore50",
            "Kr1",
            "Kr2",
            "Kr5",
            "Kr10",
            "Kr20"});
            this.coinsCB.Location = new System.Drawing.Point(40, 21);
            this.coinsCB.Name = "coinsCB";
            this.coinsCB.Size = new System.Drawing.Size(106, 21);
            this.coinsCB.TabIndex = 4;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.cancelB);
            this.groupBox1.Controls.Add(this.coinsCB);
            this.groupBox1.Controls.Add(this.insertCoinB);
            this.groupBox1.Cursor = System.Windows.Forms.Cursors.Default;
            this.groupBox1.Location = new System.Drawing.Point(12, 353);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(264, 84);
            this.groupBox1.TabIndex = 12;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Coins";
            // 
            // cancelB
            // 
            this.cancelB.Cursor = System.Windows.Forms.Cursors.Default;
            this.cancelB.Enabled = false;
            this.cancelB.Location = new System.Drawing.Point(152, 48);
            this.cancelB.Name = "cancelB";
            this.cancelB.Size = new System.Drawing.Size(106, 23);
            this.cancelB.TabIndex = 5;
            this.cancelB.Text = "Cancel";
            this.cancelB.UseVisualStyleBackColor = true;
            this.cancelB.Click += new System.EventHandler(this.cancelB_Click);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.daimATB);
            this.groupBox2.Controls.Add(this.hariboATB);
            this.groupBox2.Controls.Add(this.faxeATB);
            this.groupBox2.Controls.Add(this.cokeATB);
            this.groupBox2.Controls.Add(this.hariboB);
            this.groupBox2.Controls.Add(this.faxeB);
            this.groupBox2.Controls.Add(this.daimB);
            this.groupBox2.Controls.Add(this.cokeB);
            this.groupBox2.Controls.Add(this.purchaseB);
            this.groupBox2.Cursor = System.Windows.Forms.Cursors.Default;
            this.groupBox2.Location = new System.Drawing.Point(12, 66);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(264, 279);
            this.groupBox2.TabIndex = 13;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Stock";
            // 
            // daimATB
            // 
            this.daimATB.BackColor = System.Drawing.SystemColors.ControlLight;
            this.daimATB.Enabled = false;
            this.daimATB.Location = new System.Drawing.Point(14, 228);
            this.daimATB.Name = "daimATB";
            this.daimATB.Size = new System.Drawing.Size(20, 20);
            this.daimATB.TabIndex = 22;
            // 
            // hariboATB
            // 
            this.hariboATB.BackColor = System.Drawing.SystemColors.ControlLight;
            this.hariboATB.Enabled = false;
            this.hariboATB.Location = new System.Drawing.Point(14, 165);
            this.hariboATB.Name = "hariboATB";
            this.hariboATB.Size = new System.Drawing.Size(20, 20);
            this.hariboATB.TabIndex = 21;
            // 
            // faxeATB
            // 
            this.faxeATB.BackColor = System.Drawing.SystemColors.ControlLight;
            this.faxeATB.Enabled = false;
            this.faxeATB.Location = new System.Drawing.Point(14, 102);
            this.faxeATB.Name = "faxeATB";
            this.faxeATB.Size = new System.Drawing.Size(20, 20);
            this.faxeATB.TabIndex = 20;
            // 
            // cokeATB
            // 
            this.cokeATB.BackColor = System.Drawing.SystemColors.ControlLight;
            this.cokeATB.Enabled = false;
            this.cokeATB.Location = new System.Drawing.Point(14, 39);
            this.cokeATB.Name = "cokeATB";
            this.cokeATB.Size = new System.Drawing.Size(20, 20);
            this.cokeATB.TabIndex = 19;
            // 
            // hariboB
            // 
            this.hariboB.Cursor = System.Windows.Forms.Cursors.Default;
            this.hariboB.Image = ((System.Drawing.Image)(resources.GetObject("hariboB.Image")));
            this.hariboB.Location = new System.Drawing.Point(40, 146);
            this.hariboB.Name = "hariboB";
            this.hariboB.Size = new System.Drawing.Size(106, 57);
            this.hariboB.TabIndex = 17;
            this.hariboB.UseVisualStyleBackColor = true;
            this.hariboB.Click += new System.EventHandler(this.hariboB_Click);
            // 
            // faxeB
            // 
            this.faxeB.Cursor = System.Windows.Forms.Cursors.Default;
            this.faxeB.Image = ((System.Drawing.Image)(resources.GetObject("faxeB.Image")));
            this.faxeB.Location = new System.Drawing.Point(40, 83);
            this.faxeB.Name = "faxeB";
            this.faxeB.Size = new System.Drawing.Size(106, 57);
            this.faxeB.TabIndex = 16;
            this.faxeB.UseVisualStyleBackColor = true;
            this.faxeB.Click += new System.EventHandler(this.faxeB_Click);
            // 
            // daimB
            // 
            this.daimB.Cursor = System.Windows.Forms.Cursors.Default;
            this.daimB.Image = ((System.Drawing.Image)(resources.GetObject("daimB.Image")));
            this.daimB.Location = new System.Drawing.Point(40, 209);
            this.daimB.Name = "daimB";
            this.daimB.Size = new System.Drawing.Size(106, 57);
            this.daimB.TabIndex = 18;
            this.daimB.UseVisualStyleBackColor = true;
            this.daimB.Click += new System.EventHandler(this.daimB_Click);
            // 
            // cokeB
            // 
            this.cokeB.Cursor = System.Windows.Forms.Cursors.Default;
            this.cokeB.Image = ((System.Drawing.Image)(resources.GetObject("cokeB.Image")));
            this.cokeB.Location = new System.Drawing.Point(40, 20);
            this.cokeB.Name = "cokeB";
            this.cokeB.Size = new System.Drawing.Size(106, 57);
            this.cokeB.TabIndex = 15;
            this.cokeB.UseVisualStyleBackColor = true;
            this.cokeB.Click += new System.EventHandler(this.cokeB_Click);
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.dispL);
            this.groupBox3.Cursor = System.Windows.Forms.Cursors.Default;
            this.groupBox3.Location = new System.Drawing.Point(12, 13);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(264, 47);
            this.groupBox3.TabIndex = 14;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Display";
            // 
            // dispL
            // 
            this.dispL.AutoSize = true;
            this.dispL.BackColor = System.Drawing.SystemColors.Control;
            this.dispL.Cursor = System.Windows.Forms.Cursors.Default;
            this.dispL.Font = new System.Drawing.Font("Lucida Console", 15.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.dispL.Location = new System.Drawing.Point(100, 16);
            this.dispL.Name = "dispL";
            this.dispL.Size = new System.Drawing.Size(75, 21);
            this.dispL.TabIndex = 10;
            this.dispL.Text = "-----";
            this.dispL.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(293, 450);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Cursor = System.Windows.Forms.Cursors.Default;
            this.Name = "Form1";
            this.Text = "Vending Machine";
            this.groupBox1.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button purchaseB;
        private System.Windows.Forms.Button insertCoinB;
        private System.Windows.Forms.ComboBox coinsCB;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Label dispL;
        private System.Windows.Forms.Button hariboB;
        private System.Windows.Forms.Button faxeB;
        private System.Windows.Forms.Button cokeB;
        private System.Windows.Forms.Button daimB;
        private System.Windows.Forms.Button cancelB;
        private System.Windows.Forms.TextBox daimATB;
        private System.Windows.Forms.TextBox hariboATB;
        private System.Windows.Forms.TextBox faxeATB;
        private System.Windows.Forms.TextBox cokeATB;
    }
}

