const axios = require('axios');
const Vue = require('vue');
const csv = require('csv');

var app = new Vue({
  el: '#main',
  data: {
    fileName: '',
    words: [],
    currentWordIndex: null,
    sentences: [],
    currentSentenceIndex: null,
    features: [''],
    tokensMap: {},
    statusMessage: '',
    isDirty: false,
    defaultFeature: '名詞,一般,*,*,*,*,*,*,*'
  },
  computed: {
    fileNameIsValid() {
      return this.fileName.match(/^[a-zA-Z0-9_-]+$/) !== null;
    }
  },
  watch: {
    words() {
      this.isDirty = true;
    },
    sentences() {
      this.isDirty = true;
    },
  },
  methods: {
    addWord() {
      this.words.push({surface: '', feature: this.defaultFeature, cost: 1000});
      this.currentWordIndex = this.words.length - 1;
    },
    removeWord() {
      this.words.splice(this.currentWordIndex, 1);
    },
    fixWord() {
      this.currentWordIndex = null;
    },
    addSentence() {
      this.sentences.push('');
      this.currentSentenceIndex = this.sentences.length - 1;
    },
    removeSentence() {
      this.sentences.splice(this.currentSentenceIndex, 1);
    },
    fixSentence() {
      this.currentSentenceIndex = null;
    },
    addSentenceFromFile(e) {
      const reader = new FileReader();
      const file = e.target.files[0];
      reader.readAsText(file);
      reader.onload = () => {
        if (file.name.endsWith('.csv')) {
          csv.parse(reader.result, (err, data) => {
            if (!err) {
              this.sentences.push.apply(this.sentences,
                                        data.map(row => row[0].split('\n').join(' ')).filter(x => 0 < x.trim().length));
            }
          });
        } else {
          this.sentences.push.apply(this.sentences, reader.result.split('\n').filter(x => 0 < x.trim().length));
        }
      };
    },
    load() {
      return axios.get('/dict/' + this.fileName)
           .then((response) => {
             console.log(response);
             if (response.data.ok) {
               this.words = (response.data.words || []).map(row => {
                 const cells = row.split(',');
                 return {
                   surface: cells[0],
                   cost: +cells[3],
                   feature: cells.slice(4).join(',')
                 };
               });
               this.sentences = response.data.sentences || [];

               this.isDirty = false;
             } else {
               this.statusMessage = this.fileName + ' のロードに失敗しました';
             }
           })
           .catch((error) => {
             this.statusMessage = this.fileName + ' のロードに失敗しました';
             console.log(error);
           });
    },
    save() {
      return axios.put('/dict/' + this.fileName, {
        words: this.words.map(word => `${word.surface},,,${word.cost},${word.feature}`),
        sentences: this.sentences,
      })
           .then((response) => {
             console.log(response);
             this.statusMessage = this.fileName + ' をセーブしました';
             this.isDirty = false;
           })
           .catch((error) => {
             console.log(error);
           });
    },
    parse() {
      return axios.get('/dict/' + this.fileName + '/check')
           .then((response) => {
             this.tokensMap = {};
             for (var i = 0; i < response.data.length; i += 2) {
               this.tokensMap[response.data[i]] = response.data[i+1]
                                                          .split('\n')
                                                          .map(line => line.split('\t'))
                                                          .filter(line => 2 <= line.length);
             }
             this.statusMessage = '';
           })
           .catch((error) => {
             console.log(error);
           });
    }
  },
  mounted() {
    axios.get('/features')
         .then((response) => {
           this.features = response.data.split(/\r\n|\n|\r/);
         })
         .catch((error) => {
           console.log(response);
         });
  }
});
