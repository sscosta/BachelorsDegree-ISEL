//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated from a template.
//
//     Manual changes to this file may cause unexpected behavior in your application.
//     Manual changes to this file will be overwritten if the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace ViewController
{
    using System;
    using System.Collections.Generic;
    
    public partial class Franqueado
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public Franqueado()
        {
            this.Entrega = new HashSet<Entrega>();
            this.HistoricoVendas = new HashSet<HistoricoVendas>();
            this.ProdVendidoPorFranqueado = new HashSet<ProdVendidoPorFranqueado>();
        }
    
        public decimal id { get; set; }
        public decimal nif { get; set; }
        public string nome { get; set; }
        public string morada { get; set; }
    
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Entrega> Entrega { get; set; }
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<HistoricoVendas> HistoricoVendas { get; set; }
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<ProdVendidoPorFranqueado> ProdVendidoPorFranqueado { get; set; }
    }
}
