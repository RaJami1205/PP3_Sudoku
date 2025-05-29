using System.Diagnostics;

namespace sudoku.Services {
	public class PrologService {
		public string ConsultarProlog(string consulta) {
			Environment.SetEnvironmentVariable("Path", @"D:\Apps\Coding\swipl\bin");
			var rutaPl = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "Prolog", "sudoku.pl");

			var psi = new ProcessStartInfo
			{
				FileName = "sudoku",
				Arguments = $"-s \"{rutaPl}\" -g \"{consulta},halt.\"",
				RedirectStandardOutput = true,
				UseShellExecute = false,
				CreateNoWindow = true
			};

			using (var proceso = Process.Start(psi)) {
				string salida = proceso.StandardOutput.ReadToEnd();
				proceso.WaitForExit();
				return salida;
			}
		}
	}
}
