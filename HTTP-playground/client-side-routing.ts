import express = require('express');
import path = require('path');

const app = express();
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.html'));
  console.log(`got request on ${req.url}`);
  
});

app.listen(80, () => {
  console.log('listen at 80');
});

app.get('/index.js', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.js'));
});