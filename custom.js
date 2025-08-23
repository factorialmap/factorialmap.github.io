document.addEventListener("DOMContentLoaded", function() {
  // Seleciona os links na seção 'about'
  const links = document.querySelectorAll('.about-links a');
  links.forEach(function(link) {
    // Adiciona os atributos para abrir em nova aba
    link.setAttribute('target', '_blank');
    link.setAttribute('rel', 'noopener noreferrer');
  });
});