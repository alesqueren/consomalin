const monthToLetters = ['Janvier', 'Fevrier', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Aout', 'Septembre', 'Octobre', 'Novembre', 'Décembre'];
const dayToLetters = ['Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi', 'Dimanche'];

module.exports = {
  toFrenchTime(time) {
    const minutes = (time.getMinutes() < 10) ? ('0' + time.getMinutes()) : time.getMinutes();
    const hours = (time.getHours() < 10) ? ('0' + time.getHours()) : time.getHours();
    const day = (time.getDate() < 10) ? ('0' + time.getDate()) : time.getDate();
    const dayName = dayToLetters[time.getDay() - 1];
    const realMonth = time.getMonth() + 1;
    const month = (realMonth < 10) ? ('0' + realMonth) : realMonth;
    const monthName = monthToLetters[time.getMonth()];
    const year = (time.getFullYear() < 10) ? ('0' + time.getFullYear()) : time.getFullYear();
    return {
      hours,
      minutes,
      day,
      dayName,
      month,
      monthName,
      year,
    };
  },
};
