const groupsManager = require('./managers/groups');
const wishManager = require('./managers/wishes');

module.exports = (uid) => {
  const h1 = groupsManager.add(uid, 'Fajitas');
  setTimeout(() => {
    wishManager.add(uid, h1, 'Poulet');
    wishManager.add(uid, h1, 'Poivrons');
    wishManager.add(uid, h1, 'Emmental');
    wishManager.add(uid, h1, 'Tomates');
    groupsManager.unselect(uid, h1);
  }, 100);

  const h2 = groupsManager.add(uid, 'Gratin Dauphinois');
  setTimeout(() => {
    wishManager.add(uid, h2, 'Pomme De Terre');
    wishManager.add(uid, h2, 'Ail');
    wishManager.add(uid, h2, 'Creme Fraiche');
    wishManager.add(uid, h2, 'Beurre');
    wishManager.add(uid, h2, 'Lait');
    wishManager.add(uid, h2, 'Muscade');
  }, 100);

  const h3 = groupsManager.add(uid, 'Quiche Lorraine');
  setTimeout(() => {
    wishManager.add(uid, h3, 'Pate Brisée');
    wishManager.add(uid, h3, 'Lardons');
    wishManager.add(uid, h3, 'Beurre');
    wishManager.add(uid, h3, 'Oeufs');
    wishManager.add(uid, h3, 'Creme');
    wishManager.add(uid, h3, 'Lait');
    wishManager.add(uid, h3, 'Muscade');
  }, 100);

  const h4 = groupsManager.add(uid, 'Barbecue');
  setTimeout(() => {
    wishManager.add(uid, h4, 'Saucisses');
    wishManager.add(uid, h4, 'Ketchup');
    wishManager.add(uid, h4, 'Moutarde');
    wishManager.add(uid, h4, 'Pain');
    wishManager.add(uid, h4, 'Biere');
  }, 100);

  const h5 = groupsManager.add(uid, 'Fondant Au Chocolat');
  setTimeout(() => {
    wishManager.add(uid, h5, 'Chocolat Noir');
    wishManager.add(uid, h5, 'Oeufs');
    wishManager.add(uid, h5, 'Sucre En Poudre');
    wishManager.add(uid, h5, 'Beurre');
    wishManager.add(uid, h5, 'Farine');
  }, 100);

  const h6 = groupsManager.add(uid, 'Tiramisu');
  setTimeout(() => {
    wishManager.add(uid, h6, 'Oeufs');
    wishManager.add(uid, h6, 'Sucre');
    wishManager.add(uid, h6, 'Sucre Vanillé');
    wishManager.add(uid, h6, 'Mascarpone');
    wishManager.add(uid, h6, 'Biscuits À La Cuillere');
    wishManager.add(uid, h6, 'Café');
    wishManager.add(uid, h6, 'Cacao');
  }, 100);

  const h7 = groupsManager.add(uid, 'Fromages');
  setTimeout(() => {
    wishManager.add(uid, h7, 'Comté');
    wishManager.add(uid, h7, 'Brie');
    wishManager.add(uid, h7, 'Camembert');
  }, 100);

  const h8 = groupsManager.add(uid, 'Desserts');
  setTimeout(() => {
    wishManager.add(uid, h8, 'Yaourt Nature');
    wishManager.add(uid, h8, 'Compote');
    wishManager.add(uid, h8, 'Petit Suisse');
    wishManager.add(uid, h8, 'Glace');
  }, 100);

  const h9 = groupsManager.add(uid, 'Petit Déjeuner');
  setTimeout(() => {
    wishManager.add(uid, h9, 'Jus D\'orange');
    wishManager.add(uid, h9, 'Lait');
    wishManager.add(uid, h9, 'Poulain');
    wishManager.add(uid, h9, 'Confiture');
  }, 100);

  const h10 = groupsManager.add(uid, 'Menage');
  setTimeout(() => {
    wishManager.add(uid, h10, 'Essuie Tout');
    wishManager.add(uid, h10, 'Sac Poubelle');
    wishManager.add(uid, h10, 'Éponge');
    wishManager.add(uid, h10, 'Liquide Vaisselle');
    wishManager.add(uid, h10, 'Savon Noir');
  }, 100);

  const h11 = groupsManager.add(uid, 'Pour Bébé');
  setTimeout(() => {
    wishManager.add(uid, h11, 'Lingettes');
    wishManager.add(uid, h11, 'Pot Pomme');
    wishManager.add(uid, h11, 'Couches');
  }, 100);
};
