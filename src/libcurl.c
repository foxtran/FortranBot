#include <malloc.h>
#include <stdlib.h>
#include <curl/curl.h>
#include <string.h>

CURL *curl;

typedef struct string {
    size_t len;
    char *str;
} string;

char *proxy = PROXY;

//thanks @vehlwn for help with this code
void append(string *this, char *buffer, size_t size)
{
    size_t newSize = this->len + size;
    char *newStr = malloc(newSize);
    *newStr = 0;
    if(this->str)
        strcat(newStr, this->str);
    strncat(newStr, buffer, size);
    free(this->str);
    this->str = newStr;
    this->len = newSize;
}

size_t static write_callback_func(void *buffer, size_t size, size_t nmemb, void *userp)
{
    append(((string*)userp), buffer, size*nmemb+1);
    return size*nmemb;
}

void ccurl_get(char *result, char *url, char *message, int *len, int *status)
{
    int resp_len=0;
    printf("curlget: %s\n", url);
    printf("curlget: %s\n", message);
    CURLcode res;

    string response;
    response.len = 0;
    response.str = malloc(1);
    response.str[0] = 0;

    if(curl == NULL) curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, message);
    curl_easy_setopt(curl, CURLOPT_PROXY, proxy);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback_func);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    res = curl_easy_perform(curl);
    //curl_easy_cleanup(curl);
    *status = (int)res;
    if(res == CURLE_OK)
        while(resp_len < response.len - 1)
        {
            result[resp_len] = response.str[resp_len];
            resp_len++;
        }
    while(++resp_len < *len)
    {
        result[resp_len] = ' ';
    }
    free(response.str);

}

void ccurl_sendfile(char *result, char *url, int *count, int *keylen, char keys[*count][*keylen], int *vallen, char values[*count][*vallen], char *type, char *filepath, int *len, int *status)
{
    printf("curlsend: %s\n", url);
    int resp_len = 0;
    string response;
    response.len = 0;
    response.str = malloc(1);
    response.str[0] = 0;
    CURLcode res;
    curl_mime *form = NULL;
    curl_mimepart *field = NULL;
    if(curl == NULL) curl = curl_easy_init();

    form = curl_mime_init(curl);

    field = curl_mime_addpart(form);
    curl_mime_name(field, type);
    curl_mime_filedata(field, filepath);

    for(int i = 0; i < *count; i++)
    {
        printf("%s\n", keys[i]);
        printf("%s\n", values[i]);
        field = curl_mime_addpart(form);
        curl_mime_name(field, keys[i]);
        curl_mime_data(field, values[i], CURL_ZERO_TERMINATED);
    }

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_PROXY, proxy);
    curl_easy_setopt(curl, CURLOPT_MIMEPOST, form);
    //curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback_func);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    res = curl_easy_perform(curl);
    //curl_easy_cleanup(curl);
    *status = (int)res;
    if(res == CURLE_OK)
        while(resp_len < response.len - 1)
        {
            result[resp_len] = response.str[resp_len];
            resp_len++;
        }
    while(resp_len < *len)
    {
        result[resp_len] = ' ';
        resp_len++;
    }
    curl_mime_free(form);
    free(response.str);
}