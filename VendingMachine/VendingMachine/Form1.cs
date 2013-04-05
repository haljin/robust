using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using VendingMachine.Data;

namespace VendingMachine
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();

            
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Product test = new Product("Lol", 10, 20);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Product test = new Product("Lol", 10, -10);
        }
    }
}
