const axios = require('axios');
const Vue = require('vue');

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
    load() {
      axios.get('/dict/' + this.fileName)
           .then((response) => {
             console.log(response);
             this.words = (response.data.words || []).map(row => {
               const cells = row.split(',');
               return {
                 surface: cells[0],
                 cost: +cells[3],
                 feature: cells.slice(4).join(',')
               };
             });
             this.sentences = response.data.sentences || [];

             this.statusMessage = this.fileName + ' をロードしました';
             this.isDirty = false;
           })
           .catch((error) => {
             console.log(error);
           });
    },
    save() {
      axios.put('/dict/' + this.fileName, {
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
      axios.get('/dict/' + this.fileName + '/check')
           .then((response) => {
             this.tokensMap = response.data || {};
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
