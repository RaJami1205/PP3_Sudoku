using Microsoft.AspNetCore.Mvc.RazorPages;
using System.Text.Json;
using System.Collections.Generic;
using sudoku.Pages.Tablero;

namespace sudoku.Pages {
	public class EstadisticasModel : PageModel {
		public List<tableroModel.EstadisticasJuego> EstadisticasTodas { get; set; } = new();

		public void OnGet() {
			var json = HttpContext.Session.GetString("EstadisticasTodas");
			if (!string.IsNullOrEmpty(json)) {
				EstadisticasTodas = JsonSerializer.Deserialize<List<tableroModel.EstadisticasJuego>>(json) ?? new List<tableroModel.EstadisticasJuego>();
			}
		}
	}
}
