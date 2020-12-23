import React, { useEffect } from 'react';
import './App.css'

interface ShortBook {
  title: string,
  author: string,
}

function App() {
  useEffect(() => {
    fetch('/api/titles').then(x => {
      return x.json()
    })
    .then((xs: ShortBook[]) => {
      console.log(xs)
    })
  }, []);
  
  return (
    <div>
      <header className="App-header">
        Jenna Allred private library
      </header>
    </div>
  );
}

export default App;
