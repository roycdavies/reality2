export default class reality2_node {
    url: string;

    constructor(url: string) {
        this.url = url;
    }

    sentantAll() {
        let body = 
        `query {
            sentantAll {
              id
              name
            }
        }`;

        let parameters = {
            method: 'POST',
            headers: {
                'Content-Type': 'text/plain',
                'Accept': '*/*',
                // 'Authorization': 'Bearer ' + token
            },
            body: body
        };

        return fetch(this.url + '/reality2', parameters)
            .then(response => response.json())
            .then(data => {
                return data;
            });
    }
}